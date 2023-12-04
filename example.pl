:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).

initially(green(light)).
initially(green(car)).

initiates(change, green(light), T) :-
    holdsAt(red(light), T).
initiates(change, red(light), T) :-
    holdsAt(green(light), T).

terminates(change, red(light), T) :-
    holdsAt(red(light), T).
terminates(change, green(light), T) :-
    holdsAt(green(light), T).

% The narrative
happens(change, 1).
happens(change, 2).
happens(change, 3).
happens(change, 4).
happens(change, 5).

print_dec_state(T) :-
    TPrev is T - 1,
    format("Events that occurred at time ~w:~n", [TPrev]),
    forall(happens(E, TPrev), (print(E), nl)),
    nl,
    format("Fluents that hold at time ~w:~n", [T]),
    forall(holdsAt(F, T), (print(F), nl)),
    nl.



