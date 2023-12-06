:- use_module(library(heaps)).
:- dynamic trust/3.                     % trust(Trustor, Trustee, Negative_Log_Trust)
:- dynamic belief/3.                    % belief(Believer, Belief, Confidence)
:- dynamic optimistic/0.                % Should not be altered during computation of the narrative!

mode(Sign) :-
    \+ optimistic
        % Pessimistic by default; take the min confidence values
        -> Sign is 1
        % Optimistic; take max confidence values
        ; Sign is -1.

update_trust(Trustor, Trustee, Trust) :-
    % Remove previous trust level
    retractall(trust(Trustor, Trustee, _Old_Trust)),
    % Convert the (new) trust level into negative log trust; Logarithms have the property that log(a * b) = log(a) + log(b)
    % which is useful because Dijkstra's algorithm uses addition (not multiplication) for calculating path length
    % This also means we need Trust to have the right domain
    (
        (Trust > 0, Trust =< 1)
            -> (mode(Sign), Log_Trust is Sign * log(Trust),
                assert(trust(Trustor, Trustee, Log_Trust)))
            % trust is 0, so don't re-assert.
            ; true
    ),
    % Need to recalculate relevant confidence values of beliefs
    forall(
        % For every belief of Trustee,
        (belief(Trustee, Claim, _)),
        (
            % Redo the computation of confidence
            Claim = claim(Claimant, Statement, Time),
            belief(Claimant, Claim, Claimant_Confidence),
            update_claim(Claimant, Statement, Claimant_Confidence, Time)
        )).

% Claimant      - The entity who Claimants the claim to exist
% Statement     - The statement being asserted / claimed by Claimant
% Confidence    - How confident is the claimant in this claim?
% Time          - When does the claim apply? i.e. claims can be made about the past
update_claim(Claimant, Statement, Confidence, Time) :-
    % Get rid of all the beliefs in this claim and recompute the beliefs through the (new) trust graph
    % This is inefficient but simple and it ensures correctness
    retractall(belief(_Believer, Claim, _)),
    % Make sure Confidence is within (0, 1] (cannot include zero due to log(0) being undefined)
    (Confidence > 0, Confidence =< 1)
        ->  (% Initialise the heap
            list_to_heap([], Heap),
            % Store information in Claim about who created it, the statement it represents, and the time it holds / held
            Claim = claim(Claimant, Statement, Time),
            % Dijkstra's algorithm
            mode(Sign), Log_Confidence is Sign * log(Confidence),
            propagate_claim(node(Claimant, Claim, Log_Confidence), Heap))
        % Otherwise, Confidence is zero (or an inappropriate value); it has effectively been retracted
        ;   true.

propagate_claim(Current_Node, Heap_0) :- 
    % This current node has been chosen as the next path with least cost
    % Therefore, we have found the minimum Confidence (path cost) that 
    % Current_Node can have in Claim
    Current_Node = node(Current_Entity, Claim, Log_Confidence),
    mode(Sign), Confidence is exp(Sign * Log_Confidence),
    assert(belief(Current_Entity, Claim, Confidence)),
    % Find the neighbours' path costs
    findall(
        (Cost, node(Neighbour_Entity, Claim, Cost)),
        (
            trust(Neighbour_Entity, Current_Entity, Log_Trust),
            % Don't add it if we already know the minimum cost to that Neighbour
            \+ belief(Neighbour_Entity, Claim, _Confidence),
            Cost is Log_Trust + Log_Confidence
        ),
        New_Paths),
    % Add the New_Paths to the heap
    add_list_to_heap(Heap_0, New_Paths, Heap_1),
    % Recurse on the next minimum cost path
    (get_next_node(Heap_1, Next_Node, Heap_2))
        -> propagate_claim(Next_Node, Heap_2)
        % Otherwise, we've found all the min cost paths; we're done!
        ; true.

% We've reached the end of the list, so the heap is done being modified
add_list_to_heap(Heap, [], Heap).
% Recursive case; take next item and add it to the heap, then recurse.
add_list_to_heap(Heap_0, [Element | Tail], Heap_1) :-
    Element = (Priority, Node),
    add_to_heap(Heap_0, Priority, Node, Heap_2),
    add_list_to_heap(Heap_2, Tail, Heap_1).

% Go through the heap to find the next min cost path to a node that has not
% already been found and assigned a minimum cost.
% Fails if it does not find a new node in the heap (i.e. the heap holds no unseen nodes).
get_next_node(Heap_0, Next_Node, Heap_1) :-
    % Get the next node. If the heap is empty, this entire predicate fails.
    get_from_heap(Heap_0, _Priority, Current_Node, Heap_2)
    -> (Current_Node = node(Current_Entity, Claim, _Confidence),
        % Have we already found the minimum cost path to Current_Entity?
        (\+ belief(Current_Entity, Claim, _Smaller_Confidence))
            % If we haven't seen it, then stop recursing as we have found the next node to expand.
            -> (Heap_1 = Heap_2, Next_Node = Current_Node)
            % Otherwise, we need to keep looking through the heap
            ; get_next_node(Heap_2, Next_Node, Heap_1))
    % The heap is empty; there is no "next node" to expand.
    ; fail.



print_beliefs :-
    write("Beliefs:"),nl,
    forall(
        belief(X, Y, Z), 
        (
            Y = claim(Claimant, Statement, Time),
            tab(4), write(X), write(" believes "),
            write(Statement), write(" from "), write(Claimant),
            write(" about time "), write(Time), write(", with confidence "), write(Z), nl
        )
    ).


% A small narrative for illustrative purposes

?- retractall(belief(_, _, _)).
?- retractall(trust(_, _, _)).

?- update_trust(e2, e1, 0.5).
?- update_trust(e3, e2, 0.5).
?- update_trust(e3, e1, 0.75).
?- update_trust(e2, e3, 0.75).

?- update_claim(e1, property(object, "Q"), 1, 5).
?- print_beliefs.
?- update_trust(e3, e4, 0.5).
?- update_trust(e4, e1, 0.2).
?- print_beliefs.
?- update_trust(e5, e4, 1).
?- print_beliefs.
?- update_claim(e1, property(object, "Q"), 0, 5).
?- print_beliefs.
?- update_claim(e1, property(object, "Q"), 0.9, 5).
?- print_beliefs.
