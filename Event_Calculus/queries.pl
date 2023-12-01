:- use_module(library(ansi_term)).
% [scenarios/non_primary_producers, queries].

print_narrative :-
    ansi_format([fg(cyan)], "Printing entire narrative:~n~n", []),
    initialiseDEC,
    repeat,
    tick, print_dec_state, nl,
    narrative(Narrative), length(Narrative, Length), Length =< 1.

print_dec_state :-
    adjacent_timestamps(Timestamp, Future),
    format_time(atom(Date), '%d/%m/%Y, %H:%M', Timestamp),
    ansi_format([fg(green)], "Events that occurred at time ~w~n", [Date]),
    forall(happens(Event, Timestamp), (print(Event), nl)),
    nl,
    ansi_format([fg(green)], "Fluents that hold just after ~w~n", [Date]),
    forall(holdsAt(Fluent, Future), (print(Fluent), nl)),
    nl.

?- print_narrative.
