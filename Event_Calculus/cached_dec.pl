:- multifile initially/1.
:- multifile initiates/3.
:- multifile terminates/3.
:- multifile releases/3.
:- multifile happens/2.
:- multifile holdsIf/2.
:- dynamic holdsAtCached/2.
:- dynamic releasedAtCached/2.
:- dynamic cached/2.
:- dynamic happens/2.
:- dynamic narrative/1.
:- discontiguous initially/1.
:- discontiguous initiates/3.
:- discontiguous terminates/3.
:- discontiguous releases/3.
:- discontiguous happens/2.
:- discontiguous holdsAt/2.

% fluents which only hold while some condition is met ("state constraints")
% These are not held in cache, and are instead calculated at each timestep
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


% Store when each event occurs in order, in a dynamic narrative/1 clause
generate_narrative :- 
    retractall(narrative(_)),
    findall(Timestamp, happens(_, Timestamp), Timestamps),
    % We want the first reference point to be at 0, for initial conditions to be set
    Initial_Timestamp = 0,
    append([Initial_Timestamp], Timestamps, Narrative),
    asserta(narrative(Narrative)),
    % Cache the initial conditions
    forall((holdsAt(F,Initial_Timestamp), \+ holdsIf(F,Initial_Timestamp)), assert(holdsAtCached(F,Initial_Timestamp))).

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
    % Do not cache fluents with state constraints
    forall((holdsAt(F,T), \+ holdsIf(F,T)), assert(holdsAtCached(F,T))),
    assert(cached(T)),
    % Remove any previously cached information, and remove events from the narrative that have been simulated
    retractall(holdsAtCached(_, T_Previous)),
    retractall(releasedAtCached(_, T_Previous)),
    advance_narrative.

initialiseDEC :-
    retractall(holdsAtCached(_,_)),
    retractall(releasedAtCached(_,_)),
    retractall(cached(_)),
    generate_narrative.











