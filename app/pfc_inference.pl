:- ['pfc/pfc1.2/src/pfc'].
:- pfcNoWarnings.

% Rule - Propagate judgements through trust relations
:- add((
        holdsAt(judgement(A, Evidence, Claim)=Confidence), atm(judgement(A, Evidence, Claim)=Confidence), holdsAt(trust(B, A)=Trust)
        % Derived judgements aren't asserted to be atomic so that they won't trigger this rule, because
        % they won't derive any unique judgements through trust; all trust pairs have already been calculated.
        ==> { New_Evidence = [judgement(A, Evidence, Claim)=Confidence, trust(B, A)=Trust], New_Confidence is Confidence * Trust }, 
        holdsAt(judgement(B, New_Evidence, Claim)=New_Confidence)
)).

% Rule - Implicative judgements derive consequent when the antecedent is true
:- add((
    holdsAt(judgement(Agent, Evidence, Antecedent ==> Consequent)=Base_Confidence),
    { Base_Evidence = [judgement(Agent, Evidence, Antecedent ==> Consequent)=Base_Confidence],
    check_antecedent(Agent, [Antecedent], Base_Confidence, Base_Evidence, Total_Confidence, Total_Evidence) }
    ==> holdsAt(judgement(Agent, Total_Evidence, Consequent)=Total_Confidence)
)).

check_antecedent(Agent, [Claim | Remaining], Acc_Confidence, Acc_Evidence, Confidence, Evidence) :-
    Judgement = (judgement(Agent, _Evidence, Claim)=New_Confidence),
    pfc(holdsAt(Judgement)),
    write('2: '),write(Judgement),nl,
    Temp_Confidence is Acc_Confidence * New_Confidence,
    Temp_Evidence = [Judgement | Acc_Evidence],
    check_antecedent(Agent, Remaining, Temp_Confidence, Temp_Evidence, Confidence, Evidence).
check_antecedent(_Agent, [], Confidence, Evidence, Confidence, Evidence).



% Called when trust and atomic judgement fluents are changed, in order to re-derive judgements
update_pfc(Vertex_List, Edge_Dict, Judgements) :-
    % Run the Floyd–Warshall algorithm to get highest trust for each node pair (A,B).
    floyd_warshall(Vertex_List, Edge_Dict, Trust_Dict),
    % Update the facts in the Pfc engine
    assert_modified_trust(Trust_Dict),
    assert_modified_judgements(Judgements),
    % Extract new judgements
    % extract_new_judgements(Judgements),
    !.

assert_modified_trust(Trust_Dict) :-
    forall(get_dict(A, Trust_Dict, Inner_Dict),
        (forall(get_dict(B, Inner_Dict, Trust),
            (
                % The trust value hasn't changed since the last tick
                pfc(holdsAt(trust(A, B)=Trust))

                % Otherwise, (try) remove old trust values and add a new one *unless* Trust is 0
                ; (rem2(holdsAt(trust(A, B)=_Old_Trust)),
                    (Trust = 0 ; add(holdsAt(trust(A, B)=Trust))))
            ))
        )), !.

assert_modified_judgements(Judgements) :-
    % Update the *atomic* judgements which have been modified, i.e. their justification in Pfc is [user]
    % Derived judgements (from trust or implicative claims) should not be manipulated directly as they still have support
    forall(member(Judgement, Judgements),
        (
            Judgement = (judgement(Judge, _Evidence, Claim)=Confidence),
            Old_Judgement = (judgement(Judge, _Old_Evidence, Claim)=_Old_Confidence),
            (
                % If the judgement does not yet exist in Pfc, add it and note that it is atomic
                (\+ pfc(holdsAt(Old_Judgement))), add(holdsAt(Judgement)), add(atm(Judgement))
                % Or, if the judgement already exists in Pfc and is atomic
                ;   pfc(atm(Old_Judgement)),
                    % Then remove the existing judgement from Pfc and its atomicity
                    rem2(holdsAt(Old_Judgement)), rem2(atm(Old_Judgement)),
                    % Add the new judgement (unless Confidence is 0)
                    (Confidence = 0 ; add(holdsAt(Judgement)), add(atm(Judgement)))
                % Otherwise, this is a derived judgement, so we shouldn't manipulate it directly
                ; true
            )
        )), !.

floyd_warshall(Vertex_List, Edge_Dict, Output_Dict) :-
    % Create the dictionary of initial trust values
    initialise_dict(Vertex_List, Edge_Dict, Initial_Dict),
    % Iterate through the graph to find best paths
    loop_one(Vertex_List, Vertex_List, Initial_Dict, Output_Dict), !.

initialise_dict(Vertex_List, Edge_Dict, Initial_Dict) :-
    % Create a list of all pairs of nodes (A, B) with their trust levels
    % Note that trust defaults to 0, even if A=B, since each node already implicitly trusts itself
    findall(A/B-Weight, (member(A, Vertex_List), member(B, Vertex_List), Weight = Edge_Dict.get(A/B, 0)), Key_Value_Pairs),
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






% Queries

% update_pfc([a, b, c], _{a:_{b:0.8, c:0.2}, b:_{c:1.0}}, [judgement(c, [e], claim)=0.5]).
% forall((pfc(X), X =.. [holdsAt, J]),(write(J),nl)).
