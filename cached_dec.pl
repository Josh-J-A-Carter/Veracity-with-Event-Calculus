:- use_module(library(heaps)).
:- use_module(library(lists)).
:- multifile initially/1.
:- multifile initiates/3.
:- multifile terminates/3.
:- multifile releases/3.
:- multifile happens/2.
:- multifile holdsIf/2.
:- multifile causes/4.
:- dynamic initially/1.
:- dynamic initiates/3.
:- dynamic holdsAtCached/2.
:- dynamic releasedAtCached/2.
:- dynamic cached/1.
:- dynamic happens/2.
:- dynamic narrative/1.
:- dynamic optimistic/0.                % Should not be altered during computation of the narrative!
:- dynamic require_validation/2.
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



%%% Expectations

% Cache the violates/3 predicates which occur at time T
% update_violations(Updated_Violations, T) :-
%     adjacent_timestamps(T_Previous, T),
%     findall(Statement,
%         % Find every event at T which causes any claim Claim to be invalidated
%         (happens(Event, T_Previous),
%         violates(Event, Expectation, T_Previous),
%         % Ensure that the claim binds properly; we don't want unbound variables
%         Expectation = expectation(_Entity, Statement), require_validation(Statement, _)),
%         Unsorted_Expectations),
%     % Remove duplicates; we only want to add more requirements to each TYPE of expectation
%     sort(Unsorted_Expectations, Invalidated_Expectations),
%     forall(member(Statement, Invalidated_Expectations),
%         (
%             % Get the current claim's existing requirements, and remove them from storage
%             require_validation(Statement, Existing_Requirements),
%             retractall(require_validation(Statement, _)),
%             % Add the current timestamp as a new requirement, sort in case there are somehow duplicates, and store
%             append(Existing_Requirements, [T_Previous], Unsorted_Requirements), sort(Unsorted_Requirements, New_Requirements),
%             assert(require_validation(Expectation, New_Requirements))
%         )),
%     % Create a list of entities that have been affected, for each of the expectation types
%     findall((Statement, Entities),
%         % Get all the entities whose expectation has been invalidated, stored as a list
%         (member(Statement, Invalidated_Expectations),
%         findall(Entity,
%             (holdsAtCached(expectation(Entity, Statement)=_Confidence, T_Previous)),
%             Entities
%         )),
%         Updated_Violations).

% update_expectations(Updated_Violations, Updated_Judgements, T) :-
%     % Go through each statement which may have been updated
%     forall(member((Statement, Affected_Entities), Updated_Violations),
%         % Go through each entity which may have been affected, and update its expectation
%         (forall(member(Entity, Affected_Entities),
%             calculate_expectation(Entity, Statement, T)
%         ))
%     ),
%     % Go through each statement which may have been updated
%     forall(member((Statement, Affected_Entities), Updated_Judgements),
%         % Go through each entity which may have been affected, and update its expectation
%         (forall(member(Entity, Affected_Entities),
%             calculate_expectation(Entity, Statement, T)
%         ))
%     ).

% calculate_expectation(Entity, Statement, T) :-
%     retractall(holdsAtCached(expectation(Entity, Statement)=_Old_Confidence, T)),
%     % Get the requirements, and prepend a -1 timestamp to make sure initial judgement is checked
%     require_validation(Statement, Timestamps),
%     append([-1], Timestamps, Requirements),
%     total_confidence(Entity, Statement, Requirements, 1.0, Total_Confidence, T),
%     (Total_Confidence \= 0
%         -> assert(holdsAtCached(expectation(Entity, Statement)=Total_Confidence, T))
%         ; true).

% % Base case; there are no more requirements after this one
% % We only need to check LOWER time bound, since the future is undefined
% total_confidence(Entity, Statement, [Req | []], Accumulated_Confidence, Total_Confidence, T) :-
%     % Check for a judgement which applies after Req
%     findall(Confidence,
%         (
%             holdsAtCached(judgement(Entity, _Judge, Statement, Claim_Creation_Time)=Confidence, T),
%             Claim_Creation_Time > Req
%         ),
%         Relevant_Scores),
%     % If the list is empty, we're missing a judgement for this window, so total confidence is zero
%     (Relevant_Scores = []
%         -> Total_Confidence = 0
%         % There is indeed at least one confidence value; take the optimal one
%         ; ((optimistic
%                 -> max_list(Relevant_Scores, Optimal_Value)
%                 ; min_list(Relevant_Scores, Optimal_Value)),
%            Total_Confidence is Accumulated_Confidence * Optimal_Value)).

% % There are still requirements to satisfy
% total_confidence(Entity, Statement, Requirements, Accumulated_Confidence, Total_Confidence, T) :-
%     Requirements = [Req_1, Req_2 | Remaining_Requirements],
%     % Check for a judgement which applies between Req_1 and Req_2
%     findall(Confidence,
%         (
%             holdsAtCached(judgement(Entity, _Judge, Statement, Claim_Creation_Time)=Confidence, T),
%             Claim_Creation_Time > Req_1, Claim_Creation_Time =< Req_2
%         ),
%         Relevant_Scores),
%     % If there is no such confidence value, stop recursing and set the total confidence to zero
%     % This is because we are missing a judgement during this window of time
%     (Relevant_Scores = []
%         -> Total_Confidence = 0
%         % There is indeed at least one confidence value; take the optimal one
%         ; ((optimistic
%                 -> max_list(Relevant_Scores, Optimal_Value)
%                 ; min_list(Relevant_Scores, Optimal_Value)),
%            New_Acc is Accumulated_Confidence * Optimal_Value,
%            total_confidence(Entity, Statement, [Req_2 | Remaining_Requirements], New_Acc, Total_Confidence, T))).



%%% Trust & judgement handling

% Is confidence calculated optimistically or pessimistically?
% Should not be altered during execution
mode(Sign) :-
    \+ optimistic
        % Pessimistic by default; take the min confidence values
        -> Sign is 1
        % Optimistic; take max confidence values
        ; Sign is -1.


% From more complex judgements, we want to find the atomic judgements on which they rely
get_atomic_judgements(Judgement, Atomic_Judgements) :-
    Judgement = (judgement(_Judge, Evidence, _Claim)=_Confidence),
    (is_atomic(Evidence)
        -> Atomic_Judgements = [Judgement]
        ; (findall(
            List_Of_Judgements,
            (member(Base_Judgement, Evidence), get_atomic_judgements(Base_Judgement, List_Of_Judgements)),
            List_Of_Lists
        )),
        append(List_Of_Lists, Atomic_Judgements)).

% Is some given evidence atomic?
is_atomic([]).
is_atomic([Evidence | Remaining_Evidence]) :-
    Evidence \= (judgement(_, _, _)=_),
    is_atomic(Remaining_Evidence).

update_judgements(Updated_Judgements, T) :-
    % judgements can be updated from changing trust, changing judgements, or initial judgement conditions
    List_Of_Lists = [List_1, List_2, List_3],
    adjacent_timestamps(T_Previous, T),
    % ATOMIC judgements that have been terminated, released, or initiated directly
    findall(
        judgement(Judge, Evidence, Claim)=Confidence,
        (happens(E, T_Previous),
            (
                initiates(E, judgement(Judge, Evidence, Claim)=Confidence, T_Previous)
                ; terminates(E, judgement(Judge, Evidence, Claim)=Confidence, T_Previous)
                ; releases(E, judgement(Judge, Evidence, Claim)=Confidence, T_Previous)
            ),
            % We need Confidence to be instantiated
            \+ var(Confidence)),
        List_1
    ),
    % If Trustor's trust in Trustee changes, we need to recompute all of the ATOMIC judgements that Trustee believes
    findall(
        Judgement,
        (happens(E, T_Previous),
            (
                initiates(E, trust(Trustor, Trustee)=_, T_Previous)
                ; terminates(E, trust(Trustor, Trustee)=_, T_Previous)
                ; releases(E, trust(Trustor, Trustee)=_, T_Previous)
            ),
        % All of Trustee's ATOMIC beliefs need to be recalculated; so we need the provenance of them
        holdsAtCached(judgement(Trustee, Evidence, Claim)=Confidence, T_Previous),
        get_atomic_judgements(judgement(Trustee, Evidence, Claim)=Confidence, Atomic_Judgements),
        member(Judgement, Atomic_Judgements),
        \+ var(Confidence)),
        List_2
    ),
    % initially/1 predicates defining ATOMIC judgement/3 fluents
    ((T_Previous = 0)
        -> (List_1 = [], List_2 = [],
            findall(
            judgement(Judge, Evidence, Claim)=Confidence,
            (initially(judgement(Judge, Evidence, Claim)=Confidence),
            % We need Confidence to be instantiated
            \+ var(Confidence)),
            List_3))
        % T \= 0,
        ; List_3 = []),
    % Append into one list, sort to remove duplicates
    append(List_Of_Lists, Merged_List),
    sort(Merged_List, Judgements_To_Update),
    (Judgements_To_Update = []
        -> Updated_Judgements = []
        % For each updated judgement, propagate the changes through the network
        ; findall((Judgement, Affected_Entities),
            (member(Judgement, Judgements_To_Update),
            update_judgement(Judgement, Affected_Entities, T)),
            Updated_Judgements)).

% Does some evidence depend on a given judgement?
depends_directly([Evidence | Remaining_Evidence], Judgement) :-
    % This depends on Judgement if the current piece of Evidence is Judgement
    (Evidence = Judgement) -> true
    % OR any of the remaining pieces of evidence depend on Judgement
    ; depends_directly(Remaining_Evidence, Judgement).

% It isn't dependent if we reach the end of the list without binding Evidence = Judgement
depends_directly([], _Judgement) :- !, fail.

% Go to all of the judgements which depend (directly or transitively) on Updated_Judgement,
% and track all of the entities that were affected
retract_dependent_judgements(Updated_Judgement, [Judge | Affected_Entities], T) :-
    % First, retract the current judgement, and remember the entity that was affected
    Updated_Judgement = (judgement(Judge, _, _)=_),
    retractall(holdsAtCached(Updated_Judgement, T)),
    findall(
        Transitively_Affected,
        % Find every Dependent_Judgement which directly depends on Updated_Judgement
        (holdsAtCached(judgement(Entity, Evidence, Claim)=Confidence, T),
        depends_directly(Evidence, Updated_Judgement),
        Dependent_Judgement = (judgement(Entity, Evidence, Claim)=Confidence),
        % Recursively retract the transitively dependent judgements
        retract_dependent_judgements(Dependent_Judgement, Transitively_Affected, T)),
        List_Of_Lists),
    % Flatten the list
    append(List_Of_Lists, Affected_Entities).

% Judge                 - The entity who Judges the claim to exist
% Evidence              - Provenance of the judgement / supporting evidence
% Claim                 - The claim being asserted
% Confidence            - How confident is the Judge in this claim?
% T                     - What timestamp are we currently at in the narrative?
update_judgement(Judgement, Affected_Entities, T) :-
    % Remove all of the cached judgements which depend on / are derived from Judgement
    % This is inefficient but simple and it ensures correctness
    retract_dependent_judgements(Judgement, Removed_Entities, T),
    Judgement = (judgement(Judge, Evidence, Claim)=Confidence),
    ((Confidence > 0, Confidence =< 1)
        ->  (% Dijkstra's algorithm - note that we need confidence as a logarithm,
            % otherwise, with multiplication, we would have a diminishing path cost
            % and this leads to sub-optimal path choices (or non-termination in the worst case)
            list_to_heap([], Heap),
            mode(Sign), Log_Confidence is Sign * log(Confidence),
            propagate_judgement(node(Judge, Log_Confidence), judgement(Judge, Evidence, Claim)=Confidence, Added_Entities, T, Heap),
            append(Removed_Entities, Added_Entities, Unsorted_Entities),
            sort(Unsorted_Entities, Affected_Entities))
        % Otherwise, Confidence is zero (or an inappropriate value), so the judgement is retracted entirely
        ;   sort(Removed_Entities, Affected_Entities)).

propagate_judgement(Current_Node, Original_Judgement, [Entity | Added_Entities], T, Heap_0) :-
    % Unwrap the node/2 and judgement/3 structures
    Current_Node = node(Entity, Log_Confidence),
    Original_Judgement = (judgement(Original_Judge, _Original_Evidence, Claim)=_Original_Confidence),
    % Get confidence from Log_Confidence
    mode(Sign), Confidence is exp(Sign * Log_Confidence),
    % If this is the root node, then assert the original judgement (containing the original evidence)
    ((Original_Judge = Entity)
        -> assert(holdsAtCached(Original_Judgement, T))
        % Otherwise, it is not the root node, so the evidence is the original judgement
        % Note that this is not using transitive dependencies; we want each
        % new judgement to DIRECTLY depend on Original_Judgement (instead of the previous node's judgement)
        % because this makes it less computationally expensive to detect provenance
        ; assert(holdsAtCached(judgement(Entity, [Original_Judgement], Claim)=Confidence, T))),
    % Find the neighbours' path costs
    findall(
        (Neighbour, Cost),
        (
            holdsAtCached(trust(Neighbour, Entity)=Trust, T),
            % Don't add it if we already know the best path to that Neighbour
            % NOTE: _Existing_Evidence can bind to anything; this means that if we have Claim
            % verified through two different sets of evidence, then only one of the judgements will survive
            % This is something that needs to be dealt with in the future
            \+ holdsAtCached(judgement(Neighbour, _Existing_Evidence, Claim)=_Existing_Confidence, T),
            Log_Trust is Sign * log(Trust),
            Cost is Log_Trust + Log_Confidence
        ),
        New_Paths),
    % Add the newly found path costs to the heap, then find the next min (or max) cost node
    add_list_to_heap(Heap_0, New_Paths, Heap_1),
    (get_next_node(Heap_1, Next_Node, Original_Judgement, T, Heap_2)
        -> propagate_judgement(Next_Node, Original_Judgement, Added_Entities, T, Heap_2)
        % Otherwise, we've found all the min cost paths; we're done!
        ; Added_Entities = []).

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
get_next_node(Heap_0, Next_Node, Original_Judgement, T, Heap_1) :-
    % Get the next node. If the heap is empty, this entire predicate fails.
    get_from_heap(Heap_0, Log_Confidence, Entity, Heap_2)
    -> (Original_Judgement = (judgement(_Original_Judge, _Original_Evidence, Claim)=_Original_Confidence),
        % Have we already found the best cost path to Entity?
        % NOTE: Again, we are assuming that there are not multiple sets of evidence for the same claim
        ((\+ holdsAtCached(judgement(Entity, _Existing_Evidence, Claim)=_Existing_Confidence, T))
            % If we haven't seen it, then stop recursing as we have found the next node to expand.
            -> (Heap_1 = Heap_2, Next_Node = node(Entity, Log_Confidence))
            % Otherwise, we need to keep looking through the heap
            ; get_next_node(Heap_2, Next_Node, Original_Judgement, T, Heap_1)))
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
    cache_causes(Initial_Timestamp).

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
    % Propagate judgement changes
    update_judgements(_Updated_Judgements, T),
    % Apply implicative judgement rules to derive new judgements where necessary
    %%%%%%%%%%%%%%%%%%%%%%
    cache_causes(T),
    assert(cached(T)),
    % Forget the previous timestamp as it has just been simulated (move the narrative along by one)
    advance_narrative,
    % Remove any previously cached information
    retractall(holdsAtCached(_, T_Previous)),
    retractall(releasedAtCached(_, T_Previous)).

% Initialisation of the system
% Mode is either pessimistic or optimistic, depending on the desired behaviour for calculating confidence in judgements
initialiseDEC(Mode) :-
    retractall(holdsAtCached(_,_)),
    retractall(releasedAtCached(_,_)),
    retractall(require_validation(_,_)),
    retractall(cached(_)),
    retractall(optimistic),
    ((Mode = 'optimistic') 
        -> assert(optimistic)
        ; true),
    generate_narrative.

% The system is pessimistic by default
initialiseDEC :-
    initialiseDEC('optimistic').

