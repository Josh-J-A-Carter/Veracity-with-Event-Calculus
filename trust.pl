:- use_module(library(heaps)).
:- dynamic trust/3. % trust(Trustor, Trustee, Negative_Log_Trust)
:- dynamic belief/3. % belief(Believer, Belief, Confidence)

update_trust(Trustor, Trustee, Trust) :-
    % Remove previous trust level
    retractall(trust(Trustor, Trustee, _Old_Trust)),
    % Convert the (new) trust level into negative log trust; Logarithms have the property that log(a * b) = log(a) + log(b)
    % which is useful because Dijkstra's algorithm uses addition (not multiplication) for calculating path length
    Trust \= 0
        -> (Negative_Log_Trust is -log(Trust),
            assert(trust(Trustor, Trustee, Negative_Log_Trust)))
        % log(0) is undefined, so don't re-assert trust.
        ; true.

create_claim(Entity, Claim, Confidence) :-
    % Initialise the heap
    list_to_heap([], Heap),
    % Dijkstra's algorithm
    Negative_Log_Confidence is -log(Confidence),
    propagate_claim(node(Entity, Claim, Negative_Log_Confidence), Heap).

propagate_claim(Current_Node, Heap_0) :- 
    % This current node has been chosen as the next path with least cost
    % Therefore, we have found the minimum Confidence (path cost) that 
    % Current_Node can have in Claim
    Current_Node = node(Current_Entity, Claim, Negative_Log_Confidence),
    Confidence is exp(-Negative_Log_Confidence),
    assert(belief(Current_Entity, Claim, Confidence)),
    % Find the neighbours' path costs
    findall(
        (Neighbour_Negative_Log_Confidence, node(Neighbour_Entity, Claim, Neighbour_Negative_Log_Confidence)),
        (
            trust(Neighbour_Entity, Current_Entity, Negative_Log_Trust),
            % Don't add it if we already know the minimum cost to that Neighbour
            \+ belief(Neighbour_Entity, Claim, _Confidence),
            Neighbour_Negative_Log_Confidence is Negative_Log_Trust + Negative_Log_Confidence
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
    % Empty heap
    ; fail.


% Removing old trust / belief relations

:- retractall(belief(_, _, _)).
:- retractall(trust(_, _, _)).
:- update_trust(e2, e1, 0.5).
:- update_trust(e3, e2, 0.5).
:- update_trust(e3, e1, 0.75).

% Someone generates a claim

?- create_claim(e1, claim, 1).