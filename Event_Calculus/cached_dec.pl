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
:- dynamic derived_fluent/1.
:- discontiguous initially/1.
:- discontiguous initiates/3.
:- discontiguous terminates/3.
:- discontiguous releases/3.
:- discontiguous happens/2.
:- discontiguous holdsAt/2.

holdsAt(F,T) :-
    cached(T),
    holdsAtCached(F,T).

holdsAt(F,T) :-
    \+ cached(T),
    holdsAtNoCache(F,T).

% fluents which only hold while some condition is met ("state constraints")
% These are not held in cache, and are instead calculated at each timestep
holdsAt(F, T) :-
   holdsIf(F, T).

holdsAtNoCache(F, 0) :-
    initially(F).

holdsAtNoCache(F, T2) :-
    T2 > 0,
    T1 is T2 - 1,
    holdsAtCached(F, T1),
    \+ releasedAt(F, T1),
    \+ (happens(E, T1), terminates(E, F, T1)).

holdsAtNoCache(F, T2) :-
    T2 > 0,
    T1 is T2 - 1,
    happens(E, T1),
    initiates(E, F, T1).


releasedAt(F, T) :-
    releasedAtCached(F, T).
releasedAt(F, T2) :-
    T1 is T2 - 1,
    releasedAtCached(F, T1),
    \+ (happens(E, T1),
	( initiates(E, F, T1)
	 ; terminates(E, F, T1))).
releasedAt(F, T2) :-
    T1 is T2 - 1,
    happens(E, T1),
    releases(E, F, T1),
    assert(releasedAtCached(F, T2)). % Use a cached version?

% Support for multi-valued fluent, taken from Marek Sergot's lecture notes
terminates(E, F=_, T) :- initiates(E, F=_, T).

run(N) :-
    forall(between(0,N,T), tick(T)).

% Must be called at all time steps from 0 onwards. Records fluents that hold at T, given fluents and events at T-1
tick(T) :-
     Tm2 is T - 2,
     retractall(holdsAtCached(_, Tm2)),
     retractall(releasedAtCached(_, Tm2)),
     % Do not cache fluents with state constraints
     forall((holdsAt(F,T), \+ holdsIf(F,T)), assert(holdsAtCached(F,T))),
     assert(cached(T)).

initialiseDEC :-
    retractall(holdsAtCached(_,_)),
    retractall(releasedAtCached(_,_)),
    retractall(cached(_)).











