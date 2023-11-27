:- use_module(library(ansi_term)).

% interactive_narrative(Start, Stop) :-
%     ansi_format([fg(cyan)], "Printing narrative from time ~w to time ~w:~n~n", [Start, Stop]),
%     Tprev is Start - 1,
%     repeat,
%     T is Tprev + 1,
%     read(Input),
%     (
%         Input \= "\n"
%         ;
%         tick(T), print_dec_state(T), nl, T \= Stop
%     ).

print_narrative(Start, Stop) :-
    ansi_format([fg(cyan)], "Printing narrative from time ~w to time ~w:~n~n", [Start, Stop]),
    forall(between(Start, Stop, T), (tick(T), print_dec_state(T), nl)).

print_dec_state(T) :-
    TPrev is T - 1,
    ansi_format([fg(green)], "Events that occurred at time ~w:~n", [TPrev]),
    forall(happens(E, TPrev), (print(E), nl)),
    nl,
    ansi_format([fg(green)], "Fluents that hold at time ~w:~n", [T]),
    forall(holdsAt(F, T), (print(F), nl)),
    nl.

?- print_narrative(0, 5).

