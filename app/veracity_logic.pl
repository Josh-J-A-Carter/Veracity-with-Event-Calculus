:- ['pfc/pfc1.2/src/pfc'].
:- pfcNoWarnings.


:- multifile set_trust/4.
:- discontiguous set_trust/4.
:- dynamic set_trust/4.

:- multifile set_judgement/5.
:- discontiguous set_judgement/5.
:- dynamic set_trust/5.


% Rule - Propagate judgements through trust relations
:- add((
        fluent(judgement(A, Evidence, Claim)=Confidence), atm(judgement(A, Evidence, Claim)=Confidence), trust_cache(B, A, Trust)
        % Derived judgements aren't asserted to be atomic so that they won't trigger this rule, because
        % they won't derive any unique judgements through trust; all trust pairs have already been calculated.
        ==> { New_Evidence = [judgement(A, Evidence, Claim)=Confidence, trust(B, A)=Trust], New_Confidence is Confidence * Trust }, 
        fluent(judgement(B, New_Evidence, Claim)=New_Confidence)
)).

% Rule - Implicative judgements derive consequent when the antecedent is true
:- add((
    fluent(judgement(Agent, Evidence, Antecedent ==> Consequent)=Base_Confidence),
    { Base_Evidence = [judgement(Agent, Evidence, Antecedent ==> Consequent)=Base_Confidence],
    check_antecedent(Agent, [Antecedent], Base_Confidence, Base_Evidence, Total_Confidence, Total_Evidence) }
    ==> fluent(judgement(Agent, Total_Evidence, Consequent)=Total_Confidence)
)).

check_antecedent(Agent, [Claim | Remaining], Acc_Confidence, Acc_Evidence, Confidence, Evidence) :-
    Judgement = (judgement(Agent, _Evidence, Claim)=New_Confidence),
    pfc(fluent(Judgement)),
    Temp_Confidence is Acc_Confidence * New_Confidence,
    Temp_Evidence = [Judgement | Acc_Evidence],
    check_antecedent(Agent, Remaining, Temp_Confidence, Temp_Evidence, Confidence, Evidence).
check_antecedent(_Agent, [], Confidence, Evidence, Confidence, Evidence).

% Provides an interface for retrieving only fluents - other Pfc facts are not retrievable
veracity_fluent(F) :- pfc(fluent(F)).


veracity_tick(T) :- (update_trust(T) ; true), (update_judgements(T) ; true), !.

update_judgements(T) :-
    % Find the judgements which changed
    adjacent_timestamps(T_Previous, T),
    findall(judgement(A, B, C)=D, (happens(E, T_Previous), set_judgement(E, A, B, C, D)), Judgements),
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
    floyd_warshall(Trust_Dict),
    % Update the facts in the Pfc engine
    assert_modified_trust(Trust_Dict)), !.

assert_modified_trust(Trust_Dict) :-
    forall(get_dict(A, Trust_Dict, Inner_Dict),
        (forall(get_dict(B, Inner_Dict, Trust),
            (
                % The trust value hasn't changed since the last tick
                pfc(trust_cache(A, B, Trust))

                % Otherwise, (try) remove old trust values and add a new one *unless* Trust is 0
                ; (rem2(trust_cache(A, B, _Old_Trust)),
                    (Trust = 0 ; add(trust_cache(A, B, Trust))))
            ))
        )), !.

floyd_warshall(Output_Dict) :-
    % Find all nodes involved in trust relations
    findall(Vertex, (pfc(fluent(trust(A, B)=_)), (Vertex = A ; Vertex = B)), Unsorted_List),
    sort(Unsorted_List, Vertex_List),
    % Create the dictionary of initial trust values
    initialise_dict(Vertex_List, Initial_Dict),
    % Iterate through the graph to find best paths
    loop_one(Vertex_List, Vertex_List, Initial_Dict, Output_Dict), !.

weight(A, B, Weight) :-
    pfc(fluent(trust(A, B)=Weight)), !
    ; Weight = 0, !.

initialise_dict(Vertex_List, Initial_Dict) :-
    % Create a list of all pairs of nodes (A, B) with their trust levels
    % Note that trust defaults to 0, even if A=B, since each node already implicitly trusts itself
    findall(A/B-Weight, (member(A, Vertex_List), member(B, Vertex_List), weight(A, B, Weight)), Key_Value_Pairs),
    % Turn this into a dictionary
    create_dictionary(trust{}, Key_Value_Pairs, Initial_Dict), !.

create_dictionary(Input, [Key-Value | Remaining], Output) :-
    Intermediate = Input.put(Key, Value),
    create_dictionary(Intermediate, Remaining, Output).
create_dictionary(Output, [], Output).

% Three layers of looping
loop_one(V_Before, Vertex_List, Input_Dict, Output_Dict) :-
    V_Before = [V_One | V_After],
    loop_two(V_One, Vertex_List, Vertex_List, Input_Dict, Intermediate_Dict),
    loop_one(V_After, Vertex_List, Intermediate_Dict, Output_Dict).
loop_one([], _Vertex_List, Output_Dict, Output_Dict) :- !.

loop_two(V_One, V_Before, Vertex_List, Input_Dict, Output_Dict) :-
    V_Before = [V_Two | V_After],
    loop_three(V_One, V_Two, Vertex_List, Vertex_List, Input_Dict, Intermediate_Dict),
    loop_two(V_One, V_After, Vertex_List, Intermediate_Dict, Output_Dict).
loop_two(_V_One, [], _Vertex_List, Output_Dict, Output_Dict).

loop_three(V_One, V_Two, V_Before, Vertex_List, Input_Dict, Output_Dict) :-
    V_Before = [V_Three | V_After],
    % Note here that transitive trust is calculated multiplicatively
    % Thus we have to check if Old_Trust is *less* (not greater) than New_Trust
    Old_Trust is Input_Dict.get(V_Two/V_Three),
    New_Trust is Input_Dict.get(V_Two/V_One) * Input_Dict.get(V_One/V_Three),
    (Old_Trust < New_Trust
        -> Intermediate_Dict = Input_Dict.put(V_Two/V_Three, New_Trust)
        ; Intermediate_Dict = Input_Dict),
    loop_three(V_One, V_Two, V_After, Vertex_List, Intermediate_Dict, Output_Dict).
loop_three(_V_One, _V_Two, [], _Vertex_List, Output_Dict, Output_Dict).

