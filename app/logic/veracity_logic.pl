:- ['pfc/pfc1.2/src/pfc'].          % Forward chaining inferences
:- ['coq_communication'].           % Communicate with Coq to verify inferences
:- pfcNoWarnings.
:- use_module(library(lists)).

:- multifile set_trust/4.
:- discontiguous set_trust/4.
:- dynamic set_trust/4.

:- multifile set_judgement/5.
:- discontiguous set_judgement/5.
:- dynamic set_trust/5.

reset_veracity :- 
    % Reset inference engine - remove existing facts and rules
    pfcReset,
    % Reset the Coq communication channel
    reset_coq,
    % Rule - Propagate judgements through trust relations
    add((
        fluent(judgement(A, Proof, Claim)=Confidence), atm(judgement(A, Proof, Claim)=Confidence), trust_cache(B, A, _, Trust)
        % Derived judgements aren't asserted to be atomic so that they won't trigger this rule, because
        % they won't derive any unique judgements through trust; all trust pairs have already been calculated.
        ==> {
            % Get a fresh copy of Coq in case there are non-ground variables
            copy_term(Proof, Proof_Copy),
            % Atomic proofs are always of this form (and we know this is an atomic judgement because atm(judgement(...)))
            Proof_Copy = [assume(Evidence, _, _) | _],
            % Reconstruct the trust path as a list of Coq tactics
            reconstruct_trust_path(B, A, Evidence, Claim, [], Tactics),
            % Create the Coq proof for the new judgement, by appending the trust tactics to the old proof
            append(Tactics, Proof_Copy, New_Proof),
            New_Confidence is Confidence * Trust
        }, fluent(judgement(B, New_Proof, Claim)=New_Confidence)
    )).

reconstruct_trust_path(B, B, _, _, Path, Path).
reconstruct_trust_path(B, A, Evidence, Claim, Acc, Path) :-
    pfc(trust_cache(B, A, Previous, _)),
    pfc(trust_cache(Previous, A, _, Weight)),
    % Turn this into a Coq tactic which tells us how to get from Previous to A with a trust relation
    Tactic = trust(Previous, A, Evidence, Claim, Weight),
    reconstruct_trust_path(B, Previous, Evidence, Claim, [Tactic | Acc], Path).

% Reset the Pfc engine and assert appropriate rules 
:- reset_veracity.

% Extract fluents from Pfc
veracity_fluent(Out) :-
    pfc(fluent(F)),
    % If it's a judgement, include verification status & proof tree
    (F = (judgement(_, _, _)=_)
        ->  ground_judgement(F, judgement(Actor, Proof, Claim)=Confidence),
            verify(judgement(Actor, Proof, Claim)=Confidence, Success, Error_Location),
            Out = (judgement(Actor, proof(Success, Error_Location, Proof), Claim)=Confidence)
        % Otherwise, output the raw fluent
        ; Out = F).

% Go to the next tick, applying changes from previous events
veracity_tick(T) :- update_trust(T), update_judgements(T), !.

update_judgements(T) :-
    % Find the judgements which changed
    adjacent_timestamps(T_Previous, T),
    foreach((happens(Event, T_Previous), set_judgement(Event, Actor, Evidence, Claim, Confidence)), 
        (   
            % Atomic judgement, so it's a leaf node and has a basic proof.
            Proof = [assume(Evidence, Actor, Confidence), leaf],

            New_J = (judgement(Actor, Proof, Claim)=Confidence),
            Old_J = (judgement(Actor, _, Claim)=_),

            % New_J may (1) be added to Pfc, (2) replace Old_J, or (3) fail entirely
            (
                % If the judgement does not yet exist in Pfc, add it and note that it is atomic
                (\+ pfc(fluent(Old_J))), (Confidence = 0 ; add(fluent(New_J)), add(atm(New_J)))
                % Or, if the judgement already exists in Pfc and is atomic
                ;   pfc(atm(Old_J)),
                    % Then remove the existing judgement from Pfc and its atomicity
                    rem2(fluent(Old_J)), rem2(atm(Old_J)),
                    % Add the new judgement (unless Confidence is 0)
                    (Confidence = 0 ; add(fluent(New_J)), add(atm(New_J)))
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
                % and it also avoids issues associated with disjoint subsets of the trust graph retracting rules
                (Confidence = 0 ; add((Template)))
            ))
        )), !.


% Take an implicative claim and turn it into a judgement template - with constraints at the end to ensure variables are instantiated
% This is required since Pfc won't backtrack over arbitrary Prolog code, so we can't create one generic rule
% that handles implicative judgements and simply reruns Prolog code every time relevant judgements appear
transform_rule(Antecedent ==> Consequent, Transformed_Antecedent ==> Transformed_Consequent) :-
    % Turn the tuple of claims into a tuple of judgements
    claims_to_judgements(Antecedent, Actor, Ant_Judgements, Ant_Proof, Pure_Ant_Claim),
    % Create a template for the implicative claim - belief in this claim is required for forward chaining to occur
    Implication = (judgement(Actor, _, Antecedent ==> Consequent)=_),

    % In order for forwarding chaining to occur, belief in this judgement must be present
    % i.e. if A ==> B and a1 believes A, a1 only believes B if a1 also believes A ==> B
    Transformed_Antecedent = (fluent(Implication), Ant_Judgements),
    Total_Proof = impl_elim(node(Actor, node(Actor, Implication, Pure_Ant_Claim ==> Consequent), Pure_Ant_Claim ==> Consequent),
                            node(Actor, Ant_Proof, Pure_Ant_Claim)),

    % Consequent claim as a judgement, with combined proof of antecedent
    Transformed_Consequent = fluent(judgement(Actor, [Total_Proof], Consequent)=_),
    !.

% Claim_Tuple - input tuple with conjunctions and disjunctions of claims and constraints
% Judgement_Tuple turns claims into judgements (wrapped in fluent/1) and includes constraints
% Proof_Tuple only takes the judgements since those are the part that the veracity logic is concerned with
% Pure_Claim makes sure that any constraints {} are removed from the claim, since the veracity logic doesn't touch them
claims_to_judgements(Claim_Tuple, Actor, Judgement_Tuple, Proof_Tuple, Pure_Claim) :-
    Claim_Tuple = (Claim_L, Claim_R)
        -> claims_to_judgements(Claim_L, Actor, Judgement_L, Proof_L, Pure_Claim_L),
        claims_to_judgements(Claim_R, Actor, Judgement_R, Proof_R, Pure_Claim_R),
        Judgement_Tuple = (Judgement_L, Judgement_R), 
        (
            % The proof of left and right children could just be constraints - we don't want this since
            % the constraints aren't meant to be represented in the veracity logic.
            % Therefore, we only include the children's proofs if they aren't constraints
            (Proof_L \= {}, Proof_R \= {})
                ->  Proof_Tuple = and(node(Actor, Proof_L, Pure_Claim_L), node(Actor, Proof_R, Pure_Claim_R)),
                    Pure_Claim = (Pure_Claim_L, Pure_Claim_R)
            ; (Proof_L \= {}) -> Proof_Tuple = Proof_L, Pure_Claim = Pure_Claim_L
            ; (Proof_R \= {}) -> Proof_Tuple = Proof_R, Pure_Claim = Pure_Claim_R
            % Both left and right children are constraints, so we have to propagate this back to the caller
            ; Proof_Tuple = {}
        )
    ; Claim_Tuple = (Claim_L ; Claim_R)
        -> claims_to_judgements(Claim_L, Actor, Judgement_L, Proof_L, Pure_Claim_L),
        claims_to_judgements(Claim_R, Actor, Judgement_R, Proof_R, Pure_Claim_R),
        Judgement_Tuple = (Judgement_L ; Judgement_R), 
        (
            % Only include the children's proofs if they aren't constraints
            (Proof_L \= {}, Proof_R \= {})
                ->  Proof_Tuple = or(node(Actor, Proof_L, Pure_Claim_L), node(Actor, Proof_R, Pure_Claim_R)),
                    Pure_Claim = (Pure_Claim_L ; Pure_Claim_R)
            ; (Proof_L \= {}) -> Proof_Tuple = Proof_L, Pure_Claim = Pure_Claim_L
            ; (Proof_R \= {}) -> Proof_Tuple = Proof_R, Pure_Claim = Pure_Claim_R
            % Both left and right children are constraints, so we have to propagate this back to the caller
            ; Proof_Tuple = {}
        )
    % Base case - it's a constraint. Don't turn this into a judgement!
    % Also mark Proof_Tuple as {}, so that earlier calls to this function know to discard this proof
    ; Claim_Tuple =.. [{} | _] -> Judgement_Tuple = Claim_Tuple, Proof_Tuple = {}, Pure_Claim = {}
    % Base case - if the tuple isn't able to be decomposed by 'and' or 'or', then it's a single claim
    ; Judgement_Tuple = fluent(judgement(Actor, _, Claim_Tuple)=_),
    Proof_Tuple = node(Actor, (judgement(Actor, _, Claim_Tuple)=_), Claim_Tuple),
    Pure_Claim = Claim_Tuple.

% Ground confidence and proof, which can't always be calculated (reliably) during forward chaining due to Pfc's limitations
ground_judgement(J_In, J_Out) :-
    J_In = (judgement(Actor, Proof, Claim)=Confidence),
    % If confidence is unbound, we need to calculate that as well
    (var(Confidence)
        -> ground_proof(Proof, [], Ground_Proof, 1, Ground_Confidence)
        ; ground_proof(Proof, [], Ground_Proof, Confidence, Ground_Confidence)),
    J_Out = (judgement(Actor, Ground_Proof, Claim)=Ground_Confidence).

% Ground the Proof field of a judgement, iterating through the tactics until we find an impl_elim term
% Those tactics need to be carefully handled as they can have complicated structure
ground_proof([Term | Rest], Proof_Acc, Ground_Proof, Conf_Acc, Ground_Conf) :-
    (Term =.. [impl_elim | _]
        ->  parse_implication_term(Term, Parsed_Proof, Conf_Acc, Conf_Next),
            append(Proof_Acc, Parsed_Proof, Proof_Next)
        ;   append(Proof_Acc, [Term], Proof_Next), Conf_Acc = Conf_Next),
    ground_proof(Rest, Proof_Next, Ground_Proof, Conf_Next, Ground_Conf).
ground_proof([], Proof, Proof, Conf, Conf).

% This has to be done just before extracting the judgement because we don't know beforehand the exact method for proving it
% e.g. showing D from A \/ (B /\ C) --> D could be done by either A or (B /\ C), but Pfc doesn't let us easily handle this
% as the judgements are being added - it's easier to simply calculate which path has been taken when we need the judgement
parse_implication_term(Term, Proof, Conf_Acc, Total_Conf) :-
    % Implication
    Term = impl_elim(node(Actor, Impl_Term, Impl_Claim), node(Actor, Ant_Term, Ant_Claim))
        ->  parse_implication_term(Impl_Term, Impl_Proof, Conf_Acc, Impl_Conf),
            parse_implication_term(Ant_Term, Ant_Proof, Conf_Acc, Ant_Conf),
            (Impl_Conf =< Ant_Conf
                -> Total_Conf is Impl_Conf
                ; Total_Conf is Ant_Conf),
            collect_evidence(Impl_Proof, [], Impl_Evid),
            collect_evidence(Ant_Proof, [], Ant_Evid),
            % Can't use a comma to delimit two tuples; Impl_Claim and Ant_Claim, otherwise we run into problems...
            append([[impl_elim(Actor, Impl_Evid, Ant_Evid, Impl_Conf, Ant_Conf, Impl_Claim - Ant_Claim)],
                     Impl_Proof, Ant_Proof], Proof)
    % Conjunction
    ; Term = and(node(Actor, Left, Left_Claim), node(Actor, Right, Right_Claim))
        ->  parse_implication_term(Left, Left_Proof, Conf_Acc, Left_Conf),
            parse_implication_term(Right, Right_Proof, Conf_Acc, Right_Conf),
            (Left_Conf =< Right_Conf
                -> Total_Conf is Left_Conf
                ; Total_Conf is Right_Conf),
            collect_evidence(Left_Proof, [], Left_Evid),
            collect_evidence(Right_Proof, [], Right_Evid),
            % Can't use a comma to delimit two tuples; Left_Claim and Right_Claim, otherwise we run into problems...
            append([[and_intro(Actor, Left_Evid, Right_Evid, Left_Conf, Right_Conf, Left_Claim - Right_Claim)], 
                    Left_Proof, Right_Proof], Proof)
    % Disjunction
    ; Term = or(node(Actor, Left, Left_Claim), node(Actor, Right, Right_Claim))
        ->  % Check the left and right subtrees to see if they fail or pass
            (parse_implication_term(Left, Left_Proof, Conf_Acc, Left_Conf)
                -> Pass_L = true, collect_evidence(Left_Proof, [], Left_Evid)
                ; Pass_L = false),
            (parse_implication_term(Right, Right_Proof, Conf_Acc, Right_Conf)
                -> Pass_R = true, collect_evidence(Right_Proof, [], Right_Evid)
                ; Pass_R = false),
            % If both branches succeeded, choose the branch with the lower confidence
            ((Pass_L, Pass_R)
                -> (Left_Conf =< Right_Conf
                        ->  append([[or_intro1(Actor, Left_Evid, Left_Conf, Left_Claim)], Left_Proof], Proof), Total_Conf is Left_Conf
                        ;   append([[or_intro2(Actor, Right_Evid, Right_Conf, Right_Claim)], Right_Proof], Proof), Total_Conf is Right_Conf)
            ; Pass_L -> append([[or_intro1(Actor, Left_Evid, Left_Conf, Left_Claim)], Left_Proof], Proof), Total_Conf is Left_Conf
            ; Pass_R -> append([[or_intro2(Actor, Right_Evid, Right_Conf, Right_Claim)], Right_Proof], Proof), Total_Conf is Right_Conf
            % Both branches failed, so propagate the failure
            ; fail)
    % Judgement - base case
    ; Term = (judgement(Actor, _, Claim)=Conf)
        ->  % Look for the judgement in Pfc & bind variables where applicable
            % This fails if the judgement doesn't exist - so the failure is propagated back to the caller
            pfc(fluent(judgement(Actor, Proof_Structure, Claim)=Conf)),
            % Ground the judgement; its proof and confidence could be non-ground too if we chain implicative judgements together
            ground_judgement(judgement(_, Proof_Structure, _)=Conf, judgement(_, Proof, _)=Conf_Temp),
            % Update the new smallest confidence
            (Conf_Temp < Conf_Acc
                -> Total_Conf is Conf_Temp
                ; Total_Conf is Conf_Acc)
    % 'Node' term needs to be unwrapped
    ; Term = node(_, Inner, _)
        -> parse_implication_term(Inner, Proof, Conf_Acc, Total_Conf).

collect_evidence([Tactic | Rest], Evid_Acc, Evid_Total) :-
    % Collect all the atomic evidence - only from 'assume' tactics for simplicity 
    (
        Tactic = assume(Evid, _, _)
            -> New_Evid = [Evid]
            ; New_Evid = []
    ),
    append(Evid_Acc, New_Evid, Evid_Next),
    collect_evidence(Rest, Evid_Next, Evid_Total).
collect_evidence([], Evid_Total, Evid_Total).

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

