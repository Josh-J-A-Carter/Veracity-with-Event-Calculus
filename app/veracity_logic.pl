:- ['pfc/pfc1.2/src/pfc'].
:- pfcNoWarnings.

:- multifile set_trust/4.
:- discontiguous set_trust/4.
:- dynamic set_trust/4.

:- multifile set_judgement/5.
:- discontiguous set_judgement/5.
:- dynamic set_trust/5.

reset_veracity :- 
    pfcReset,
    % Rule - Propagate judgements through trust relations
    add((
        fluent(judgement(A, Evidence, Claim)=Confidence), atm(judgement(A, Evidence, Claim)=Confidence), trust_cache(B, A, _, Trust)
        % Derived judgements aren't asserted to be atomic so that they won't trigger this rule, because
        % they won't derive any unique judgements through trust; all trust pairs have already been calculated.
        ==> { reconstruct_trust_path(B, A, [], Path), New_Evidence = [judgement(A, Evidence, Claim)=Confidence | Path], 
        New_Confidence is Confidence * Trust }, fluent(judgement(B, New_Evidence, Claim)=New_Confidence)
    )).

reconstruct_trust_path(B, B, Reversed_Path, Path) :- reverse(Reversed_Path, Path), !.
reconstruct_trust_path(B, A, Acc, Path) :-
    pfc(trust_cache(B, A, Previous, _)),
    pfc(trust_cache(Previous, A, _, Trust)),
    Temp = [trust(Previous, A)=Trust | Acc],
    reconstruct_trust_path(B, Previous, Temp, Path).

% Reset the Pfc engine and assert appropriate rules
:- reset_veracity.


veracity_fluent(F) :- pfc(fluent(F)).

veracity_tick(T) :- (update_trust(T) ; true), (update_judgements(T) ; true), !.

update_judgements(T) :-
    % Find the judgements which changed
    adjacent_timestamps(T_Previous, T),
    findall(judgement(A, [B], C)=D, (happens(E, T_Previous), set_judgement(E, A, B, C, D)), Judgements),
    % Update the *atomic* judgements which have been modified
    % Derived judgements should not be manipulated directly as they still have support
    forall(member(Judgement, Judgements),
        (
            Judgement = (judgement(Agent, _Evidence, Claim)=Confidence),
            Old_Judgement = (judgement(Agent, _Old_Evidence, Claim)=_Old_Confidence),
            (
                % If the judgement does not yet exist in Pfc, add it and note that it is atomic
                (\+ pfc(fluent(Old_Judgement))), add(fluent(Judgement)), add(atm(Judgement))
                % Or, if the judgement already exists in Pfc and is atomic
                ;   pfc(atm(Old_Judgement)),
                    % Then remove the existing judgement from Pfc and its atomicity
                    rem2(fluent(Old_Judgement)), rem2(atm(Old_Judgement)),
                    % Add the new judgement (unless Confidence is 0)
                    (Confidence = 0 ; add(fluent(Judgement)), add(atm(Judgement)))
                % Otherwise, this is a derived judgement, so we shouldn't manipulate it directly
                ; fail
            ),
            % If this is not an implicative judgement, finish execution here
            (\+ Claim =.. [==> | _]
            ;
            % Otherwise, we want to make sure that the actual forward chaining rule is dealt with (not just the fluent representation)
            % Note: we can't simply make a general Pfc rule to deal with all possible implications,
            % due to the limitations with backtracking over arbitrary Prolog code.
            (
                % Process the claim, turning each claim into a judgement template, and placing constraints after judgements
                % to ensure that variables are fully bound before instantiation.
                transform_rule(Claim, Template),
                % Add the rule to Pfc, unless confidence is 0
                % Note that we don't remove the rule, since one of the conditions of implicative judgements
                % is that the rule itself is held to be true. Thus, removing belief in the rule is enough,
                % and it also prevents any issues associated with disjoint subsets of the trust graph retracting rules
                (Confidence = 0 ; add((Template)))
            ))
        )), !.

% Take an implicative claim and turn it into a judgement template - with constraints at the end to ensure variables are instantiated
% This is required since Pfc won't backtrack over arbitrary Prolog code, so we can't create one generic rule
% that handles implicative judgements and simply reruns Prolog code every time relevant judgements appear
transform_rule(Antecedent ==> Consequent, Transformed_Antecedent ==> Transformed_Consequent) :-
    % Need to turn tuple into a list so that it can be rearranged
    % Constraints need to be at the end, otherwise variables won't be grounded
    tuple_to_list(Antecedent, Antecedent_List),
    partition(is_constraint, Antecedent_List, Constraints, Claims),
    % Including the implicative claim as it triggers the rule, so belief in it is a condition
    claims_to_judgements([Antecedent ==> Consequent | Claims], Agent, Judgements_Template, Total_Evidence, Total_Confidence),
    Total_Constraints = [{ Confidence is Total_Confidence } | Constraints],
    % Turn the judgements back into a tuple
    append(Judgements_Template, Total_Constraints, Combined_Antecedent),
    tuple_to_list(Transformed_Antecedent, Combined_Antecedent),
    % Transform consequent, using total evidence and total confidence
    Transformed_Consequent = fluent(judgement(Agent, Total_Evidence, Consequent)=Confidence), !.

is_constraint(Term) :- Term =.. [{} | _].

% List of claims -> list of judgement templates, accumulating them as evidence and their confidence
claims_to_judgements([Claim | Remaining], Agent, [fluent(J) | Judgements], [J | Evidence], C * Confidence) :-
    J = (judgement(Agent, _Evidence, Claim)=C),
    claims_to_judgements(Remaining, Agent, Judgements, Evidence, Confidence).
claims_to_judgements([], _, [], [], 1).

tuple_to_list((A, B), [A | C]) :- tuple_to_list(B, C), !.
tuple_to_list(A, [A]) :- !.


update_trust(T) :-
    % Find changed trust fluents
    adjacent_timestamps(T_Previous, T),
    findall(trust(A, B)=C, (happens(E, T_Previous), set_trust(E, A, B, C)), Changed_Trust),
    % Only continue if any trust fluents changed
    (length(Changed_Trust, Length), Length = 0
    ; 
    % Assert the changed fluents and remove any old ones
    forall(member(trust(A, B)=C, Changed_Trust), (rem2(fluent(trust(A, B)=_C)), add(fluent(trust(A, B)=C)))),
    % Run the Floyd–Warshall algorithm to get highest trust for each node pair (A,B).
    floyd_warshall(Trust_Dict, Path_Dict),
    % Update the facts in the Pfc engine
    assert_modified_trust(Trust_Dict, Path_Dict)), !.

assert_modified_trust(Trust_Dict, Path_Dict) :-
    forall(get_dict(A, Trust_Dict, Inner_Dict),
        (forall(get_dict(B, Inner_Dict, Trust),
            (
                Previous = Path_Dict.get(A/B),
                % The trust value hasn't changed since the last tick
                (pfc(trust_cache(A, B, Previous, Trust))

                % Otherwise, (try) remove old trust values and add a new one *unless* Trust is 0
                ; (rem2(trust_cache(A, B, _Previous, _Old_Trust)),
                    (Trust = 0 ; add(trust_cache(A, B, Previous, Trust)))))
            ))
        )), !.

floyd_warshall(Trust_Dict, Path_Dict) :-
    % Find all nodes involved in trust relations
    findall(Vertex, (pfc(fluent(trust(A, B)=_)), (Vertex = A ; Vertex = B)), Unsorted_List),
    sort(Unsorted_List, Vertex_List),
    % Create a list of all pairs of nodes (A, B) with their trust levels
    % Note that trust defaults to 0, even if A=B, since each node already implicitly trusts itself
    findall(A/B-Weight, (member(A, Vertex_List), member(B, Vertex_List), weight(A, B, Weight)), Key_Value_Trust),
    create_dictionary(trust{}, Key_Value_Trust, Input_Trust_Dict), !,
    % Create a dictionary of the directly previous node of each vertex A with each other vertex B
    findall(A/B-Previous, (member(A, Vertex_List), member(B, Vertex_List), previous(A, B, Previous)), Key_Value_Path),
    create_dictionary(path{}, Key_Value_Path, Input_Path_Dict), !,
    % Iterate through the graph to find best paths
    loop_one(Vertex_List, Vertex_List, Input_Trust_Dict, Input_Path_Dict, Trust_Dict, Path_Dict), !.

weight(A, B, Weight) :-
    pfc(fluent(trust(A, B)=Weight)), !
    ; Weight = 0, !.

previous(A, B, Previous) :-
    pfc(fluent(trust(A, B)=_)), Previous = A, !
    ; Previous = none, !.

create_dictionary(Input, [Key-Value | Remaining], Output) :-
    Intermediate = Input.put(Key, Value),
    create_dictionary(Intermediate, Remaining, Output).
create_dictionary(Output, [], Output).

% Three layers of looping
% Note - Input_Trust_Dict and Input_Path_Dict are used as accumulators
loop_one(V_Before, Vertex_List, Input_Trust_Dict, Input_Path_Dict, Trust_Dict, Path_Dict) :-
    V_Before = [V_One | V_After],
    loop_two(V_One, Vertex_List, Vertex_List, Input_Trust_Dict, Input_Path_Dict, Mid_Trust_Dict, Mid_Path_Dict),
    loop_one(V_After, Vertex_List, Mid_Trust_Dict, Mid_Path_Dict, Trust_Dict, Path_Dict).
loop_one([], _Vertex_List, Trust_Dict, Path_Dict, Trust_Dict, Path_Dict) :- !.

loop_two(V_One, V_Before, Vertex_List, Input_Trust_Dict, Input_Path_Dict, Trust_Dict, Path_Dict) :-
    V_Before = [V_Two | V_After],
    loop_three(V_One, V_Two, Vertex_List, Vertex_List, Input_Trust_Dict, Input_Path_Dict, Mid_Trust_Dict, Mid_Path_Dict),
    loop_two(V_One, V_After, Vertex_List, Mid_Trust_Dict, Mid_Path_Dict, Trust_Dict, Path_Dict).
loop_two(_V_One, [], _Vertex_List, Trust_Dict, Path_Dict, Trust_Dict, Path_Dict).

loop_three(V_One, V_Two, V_Before, Vertex_List, Input_Trust_Dict, Input_Path_Dict, Trust_Dict, Path_Dict) :-
    V_Before = [V_Three | V_After],
    % This allows us to reconstruct the path later
    Previous = Input_Path_Dict.get(V_One/V_Three),
    % Note here that trust is combined multiplicatively
    % Thus we have to check if Old_Trust is *less* (not greater) than New_Trust
    Old_Trust is Input_Trust_Dict.get(V_Two/V_Three),
    New_Trust is Input_Trust_Dict.get(V_Two/V_One) * Input_Trust_Dict.get(V_One/V_Three),
    (Old_Trust < New_Trust
        -> Mid_Trust_Dict = Input_Trust_Dict.put(V_Two/V_Three, New_Trust),
           Mid_Path_Dict = Input_Path_Dict.put(V_Two/V_Three, Previous)
        ; Mid_Trust_Dict = Input_Trust_Dict, Mid_Path_Dict = Input_Path_Dict),
    loop_three(V_One, V_Two, V_After, Vertex_List, Mid_Trust_Dict, Mid_Path_Dict, Trust_Dict, Path_Dict).
loop_three(_V_One, _V_Two, [], _Vertex_List, Trust_Dict, Path_Dict, Trust_Dict, Path_Dict).

