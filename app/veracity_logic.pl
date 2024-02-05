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
    )),
    % Rule - Implicative judgements derive consequent when the antecedent is true
    add((
        fluent(judgement(Agent, Evidence, Antecedent ==> Consequent)=Base_Confidence),
        { Base_Evidence = [judgement(Agent, Evidence, Antecedent ==> Consequent)=Base_Confidence],
        check_antecedent(Agent, [Antecedent], Base_Confidence, Base_Evidence, Total_Confidence, Total_Evidence) }
        ==> fluent(judgement(Agent, Total_Evidence, Consequent)=Total_Confidence)
    )).

reconstruct_trust_path(B, B, Reversed_Path, Path) :- reverse(Reversed_Path, Path), !.
reconstruct_trust_path(B, A, Acc, Path) :-
    pfc(trust_cache(B, A, Previous, _)),
    pfc(trust_cache(Previous, A, _, Trust)),
    Temp = [trust(Previous, A)=Trust | Acc],
    reconstruct_trust_path(B, Previous, Temp, Path).

check_antecedent(Agent, [Claim | Remaining], Acc_Confidence, Acc_Evidence, Confidence, Evidence) :-
    Judgement = (judgement(Agent, _Evidence, Claim)=New_Confidence),
    pfc(fluent(Judgement)),
    Temp_Confidence is Acc_Confidence * New_Confidence,
    Temp_Evidence = [Judgement | Acc_Evidence],
    check_antecedent(Agent, Remaining, Temp_Confidence, Temp_Evidence, Confidence, Evidence).
check_antecedent(_Agent, [], Confidence, Evidence, Confidence, Evidence).

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
            Judgement = (judgement(Judge, _Evidence, Claim)=Confidence),
            Old_Judgement = (judgement(Judge, _Old_Evidence, Claim)=_Old_Confidence),
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
                ; true
            )
        )), !.

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

