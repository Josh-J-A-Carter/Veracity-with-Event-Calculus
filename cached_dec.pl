:- use_module(library(heaps)).
:- multifile initially/1.
:- multifile initiates/3.
:- multifile terminates/3.
:- multifile releases/3.
:- multifile happens/2.
:- multifile holdsIf/2.
:- multifile causes/4.
:- dynamic holdsAtCached/2.
:- dynamic releasedAtCached/2.
:- dynamic cached/2.
:- dynamic happens/2.
:- dynamic narrative/1.
:- dynamic optimistic/0.                % Should not be altered during computation of the narrative!
:- discontiguous initially/1.
:- discontiguous initiates/3.
:- discontiguous terminates/3.
:- discontiguous releases/3.
:- discontiguous happens/2.
:- discontiguous holdsAt/2.
:- discontiguous holdsIf/2.
:- discontiguous causes/4.

%%% Basic Event Calculus implementation

holdsAt(F, T) :-
   holdsIf(F, T).

holdsAt(F,T) :-
    cached(T),
    holdsAtCached(F,T).

holdsAt(F,T) :-
    \+ cached(T),
    holdsAtNoCache(F,T).

holdsAtNoCache(F, 0) :-
    initially(F).

holdsAtNoCache(F, T_Current) :-
    adjacent_timestamps(T_Previous, T_Current),
    holdsAtCached(F, T_Previous),               % Did F hold in the previous time slice?
    \+ (                                        % Was the fluent terminated or released since then?
        happens(E, T_Previous), 
        (
            terminates(E, F, T_Previous) ;
            releases(E, F, T_Previous)
        )
    ).

holdsAtNoCache(F, T_Current) :-
    adjacent_timestamps(T_Previous, T_Current),
    happens(E, T_Previous),
    initiates(E, F, T_Previous).


releasedAt(F, T) :-
    releasedAtCached(F, T).

releasedAt(F, T_Current) :-
    adjacent_timestamps(T_Previous, T_Current),
    releasedAtCached(F, T_Previous),
    \+ (
        happens(E, T_Previous),
        (
            terminates(E, F, T_Previous) ;
            initiates(E, F, T_Previous)
        )
    ).

releasedAt(F, T_Current) :-
    adjacent_timestamps(T_Previous, T_Current),
    happens(E, T_Previous),
    releases(E, F, T_Previous),
    assert(releasedAtCached(F, T_Current)).


% Support for multi-valued fluent, taken from Marek Sergot's lecture notes
terminates(E, F=_, T) :- initiates(E, F=_, T).

% Cache events that are caused by other events
cache_causes(T_Current) :-
    findall(
        T_Future,
        (
            % Are there any new events which cause other events to occur in the future?
            happens(E1, T_Current), causes(E1, T_Current, E2, T_Future), T_Future >= T_Current,
            % If yes, we add them as their own events
            assert(happens(E2, T_Future))
        ),
        New_Events
    ),
    % If there are new events, add them to the narrative
    New_Events \= []
    -> (
        narrative(Old_Narrative),
        append(New_Events, Old_Narrative, Unsorted_Narrative),
        sort(Unsorted_Narrative, New_Narrative),
        retractall(narrative(_)),
        asserta(narrative(New_Narrative))
    )
    % Otherwise, there are no new events so do nothing
    ; true.

% Fluents that currently hold (unless they have state constraints; we don't want to cache those)
cache_holdsAt(T) :-
    forall((holdsAt(F,T), \+ holdsIf(F,T)), assert(holdsAtCached(F,T))).



%%% Trust & belief handling

% Is confidence calculated optimistically or pessimistically?
% Should not be altered during execution
mode(Sign) :-
    \+ optimistic
        % Pessimistic by default; take the min confidence values
        -> Sign is 1
        % Optimistic; take max confidence values
        ; Sign is -1.

update_beliefs(T) :-
    % Beliefs can be updated from changing trust, changing beliefs, or initial belief conditions
    List_Of_Lists = [List_1, List_2, List_3],
    % Beliefs that have been terminated, released, or initiated directly
    findall(
        (Claimant, Claim, Creation_Time, Confidence),
        (happens(E, T),
            (
                initiates(E, belief(Claimant, Claimant, Claim, Creation_Time)=Confidence, T)
                ; terminates(E, belief(Claimant, Claimant, Claim, Creation_Time)=Confidence, T)
                ; releases(E, belief(Claimant, Claimant, Claim, Creation_Time)=Confidence, T)
            ),
            % We need Confidence to be instantiated
            \+ var(Confidence)),
        List_1
    ),
    % If Trustor's trust in Trustee changes, we need to recompute all of the beliefs that Trustee believes
    findall(
        (Claimant, Claim, Creation_Time, Confidence),
        (happens(E, T),
            (
                initiates(E, trust(Trustor, Trustee), T)
                ; terminates(E, trust(Trustor, Trustee), T)
                ; releases(E, trust(Trustor, Trustee), T)
            ),
        holdsAtCached(belief(Trustee, Claimant, Claim, Creation_Time)=_Confidence, T),
        % We need the confidence level of the source of this claim
        holdsAtCached(belief(Claimant, Claimant, Claim, Creation_Time)=Confidence, T),
        % We need Confidence to be instantiated
        \+ var(Confidence)),
        List_2
    ),
    % initially/1 predicates defining belief/4 fluents
    ((T = 0)
        -> findall(
            (Claimant, Claim, Creation_Time, Confidence),
            (initially(belief(Claimant, Claimant, Claim, Creation_Time)=Confidence),
            % We need Confidence to be instantiated
            \+ var(Confidence)),
            List_3)
        ; List_3 = []
    ),
    % Append into one list and remove any duplicates
    append(List_Of_Lists, Merged_List),
    sort(Merged_List, Beliefs_To_Update),
    length(Beliefs_To_Update, Length),
    (Length = 0
        -> true
        % For each updated claim, propagate the changes through the network
        ; (forall(member(Args, Beliefs_To_Update),
            (Args = (Claimant, Claim, Creation_Time, Confidence),
                update_claim(Claimant, Claim, Creation_Time, Confidence, T))))).

% Claimant              - The entity who Claimants the claim to exist
% Claim                 - The claim being asserted
% Confidence            - How confident is the claimant in this claim?
% Claim_Creation_Time   - When was the claim verified / created?
%                           There may be multiple instances of the same type of claim, at different points in time
% T                     - What timestamp are we currently at in the narrative?
update_claim(Claimant, Claim, Claim_Creation_Time, Confidence, T) :-
    % Remove all of the cached 'belief/4' fluents so we can recompute them using Dijkstra's algorith
    % This is inefficient but simple and it ensures correctness
    retractall(holdsAtCached(belief(_Believer, Claimant, Claim, Claim_Creation_Time), T)),
    (Confidence > 0, Confidence =< 1)
        ->  (% Dijkstra's algorithm - note that we need confidence as a logarithm!
            list_to_heap([], Heap),
            mode(Sign), Log_Confidence is Sign * log(Confidence),
            propagate_claim(node(Claimant, Log_Confidence), claim(Claimant, Claim, Claim_Creation_Time), T, Heap))
        % Otherwise, Confidence is zero (or an inappropriate value), so the claim is retracted entirely
        ;   true.

propagate_claim(Current_Node, Claim_Info, T, Heap_0) :- 
    % Unwrap the node/2 and claim/3 structures
    Current_Node = node(Entity, Log_Confidence),
    Claim_Info = claim(Claimant, Claim, Claim_Creation_Time),
    % Turn log confidence into normal confidence (with sign depending on pessimistic / optimistic behaviour)
    mode(Sign), Confidence is exp(Sign * Log_Confidence),
    % Cache this as a fluent
    assert(holdsAtCached(belief(Entity, Claimant, Claim, Claim_Creation_Time)=Confidence, T)),
    % Find the neighbours' path costs
    findall(
        (Neighbour, Cost),
        (
            holdsAtCached(trust(Neighbour, Entity)=Trust, T),
            % Don't add it if we already know the minimum cost to that Neighbour
            \+ holdsAtCached(belief(Neighbour, Claimant, Claim, Claim_Creation_Time)=_Confidence, T),
            Log_Trust is Sign * log(Trust),
            Cost is Log_Trust + Log_Confidence
        ),
        New_Paths),
    % Add the newly found path costs to the heap, then find the next min (or max) cost node
    add_list_to_heap(Heap_0, New_Paths, Heap_1),
    (get_next_node(Heap_1, Next_Node, Claim_Info, T, Heap_2))
        -> propagate_claim(Next_Node, Claim_Info, T, Heap_2)
        % Otherwise, we've found all the min cost paths; we're done!
        ; true.

% Base case; empty list.
add_list_to_heap(Heap, [], Heap).
% Recursive case; the list isn't empty, add to the heap.
add_list_to_heap(Heap_0, [Node | Tail], Heap_1) :-
    Node = (Entity, Log_Confidence),
    % Add to heap, with Log_Confidence as the priority, and Entity as the key
    add_to_heap(Heap_0, Log_Confidence, Entity, Heap_2),
    add_list_to_heap(Heap_2, Tail, Heap_1).

% Go through the heap to find the next min cost path to a node that has not
% already been found and assigned a minimum/maximum cost.
% Fails if it does not find a new node in the heap (i.e. the heap holds no unseen nodes).
get_next_node(Heap_0, Next_Node, Claim_Info, T, Heap_1) :-
    % Get the next node. If the heap is empty, this entire predicate fails.
    get_from_heap(Heap_0, Log_Confidence, Entity, Heap_2)
    -> (Claim_Info = claim(Claimant, Claim, Claim_Creation_Time),
        % Have we already found the best cost path to Entity?
        ((\+ holdsAtCached(belief(Entity, Claimant, Claim, Claim_Creation_Time)=_Confidence, T))
            % If we haven't seen it, then stop recursing as we have found the next node to expand.
            -> (Heap_1 = Heap_2, Next_Node = node(Entity, Log_Confidence))
            % Otherwise, we need to keep looking through the heap
            ; get_next_node(Heap_2, Next_Node, Claim_Info, T, Heap_1)))
    % The heap is empty; there is no "next node" to expand.
    ; fail.



%%% Narrative manipulation & setup

% Store when each event occurs in order, in a dynamic narrative/1 clause
generate_narrative :- 
    retractall(narrative(_)),
    findall(Timestamp, happens(_, Timestamp), Timestamps),
    % We want the first reference point to be at 0, for initial conditions to be set
    Initial_Timestamp = 0,
    append([Initial_Timestamp], Timestamps, Event_Timings),
    % Remove duplicate items, and make sure items are in order
    sort(Event_Timings, Narrative),
    asserta(narrative(Narrative)),
    % Cache the initial conditions
    cache_holdsAt(Initial_Timestamp),
    % Cache any newly caused events
    cache_causes(Initial_Timestamp),
    % Propagate belief changes, if applicable
    update_beliefs(Initial_Timestamp).

% Get rid of the most recent timestamp in the narrative; it will no longer be needed
advance_narrative :-
    narrative([_Previous_Event | New_Narrative]),
    retractall(narrative(_)),
    asserta(narrative(New_Narrative)).

% First element of the narrative is the previous timestamp, and the second is the current timestamp.
adjacent_timestamps(Previous_Timestamp, Current_Timestamp) :-
    narrative([Previous_Timestamp, Current_Timestamp | _Future]).
% We have no more events at the moment, so don't attempt to predict the future
adjacent_timestamps(Previous_Timestamp, _Future) :-
    narrative([Previous_Timestamp | []]).

% Tick to the next event
tick :-
    adjacent_timestamps(T_Previous, T),
    % Cache the fluents that currently hold
    cache_holdsAt(T),
    % Propagate belief changes, if applicable
    update_beliefs(T),
    % Cache any newly caused events
    cache_causes(T),
    assert(cached(T)),
    % Forget the previous timestamp as it has just been simulated (move the narrative along by one)
    advance_narrative,
    % Remove any previously cached information
    retractall(holdsAtCached(_, T_Previous)),
    retractall(releasedAtCached(_, T_Previous)).

% Initialisation of the system
% Mode is either pessimistic or optimistic, depending on the desired behaviour for calculating confidence in beliefs
initialiseDEC(Mode) :-
    retractall(holdsAtCached(_,_)),
    retractall(releasedAtCached(_,_)),
    retractall(cached(_)),
    retractall(optimistic),
    ((Mode = 'optimistic') 
        -> assert(optimistic)
        ; true),
    generate_narrative.

% The system is pessimistic by default
initialiseDEC :-
    initialiseDEC('pessimistic').

