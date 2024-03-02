:- use_module(library(heaps)).
:- use_module(library(lists)).
:- [veracity_logic].

:- multifile initially/1.
:- dynamic initially/1.
:- discontiguous initially/1.

:- multifile initiates/3.
:- dynamic initiates/3.
:- discontiguous initiates/3.

:- multifile terminates/3.
:- dynamic terminates/3.
:- discontiguous terminates/3.

:- multifile releases/3.
:- dynamic releases/3.
:- discontiguous releases/3.

:- multifile happens/2.
:- dynamic happens/2.
:- discontiguous happens/2.

:- dynamic holdsAtCached/2.
:- dynamic releasedAtCached/2.
:- dynamic cached/1.
:- dynamic narrative/1.
:- discontiguous holdsAt/2.

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

% Fluents that currently hold
cache_holdsAt(T) :-
    forall(holdsAt(F,T), assert(holdsAtCached(F,T))).


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
    % Update the veracity logic engine
    veracity_tick(Initial_Timestamp),
    assert(cached(Initial_Timestamp)).

% Get rid of the most recent timestamp in the narrative; it will no longer be needed
advance_narrative :-
    narrative([_Previous_Event | New_Narrative]),
    retractall(narrative(_)),
    asserta(narrative(New_Narrative)).

% First element of the narrative is the previous timestamp, and the second is the current timestamp.
adjacent_timestamps(Previous_Timestamp, Current_Timestamp) :-
    narrative([Previous_Timestamp, Current_Timestamp | _Future]).
% We have no more events, but need to display the fluents which were affected by Previous_Timestamp
adjacent_timestamps(Previous_Timestamp, Future) :-
    narrative([Previous_Timestamp | []]),
    Future is Previous_Timestamp + 0.001.
% We want to have initial judgements propagated when the initial timestamp is calculated
adjacent_timestamps(-1, 0).

fluent(F, T) :- holdsAt(F, T) ; veracity_fluent(F).

% Tick to the next event
tick :-
    adjacent_timestamps(T_Previous, T),
    % Cache the fluents that currently hold
    cache_holdsAt(T),
    % Tick the veracity logic engine
    veracity_tick(T),
    assert(cached(T)),
    % Forget the previous timestamp as it has just been simulated (move the narrative along by one)
    advance_narrative,
    % Remove any previously cached information
    retractall(holdsAtCached(_, T_Previous)),
    retractall(releasedAtCached(_, T_Previous)).

% Initialisation of the system
initialiseDEC :-
    retractall(holdsAtCached(_,_)),
    retractall(releasedAtCached(_,_)),
    retractall(cached(_)),
    reset_veracity,
    generate_narrative.

