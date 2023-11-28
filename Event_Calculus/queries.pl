:- use_module(library(ansi_term)).

print_narrative :-
    ansi_format([fg(cyan)], "Printing entire narrative:~n~n", []),
    initialiseDEC,
    narrative(Narrative), length(Narrative, Length),
    Limit is Length,
    forall(between(0, Limit, _Iteration), (tick, print_dec_state, nl)).

print_dec_state :-
    adjacent_timestamps(Timestamp, _Future),
    format_time(atom(Date), '%d/%m/%Y, %H:%M', Timestamp),
    ansi_format([fg(green)], "Events that occurred at time ~w~n", [Date]),
    forall(happens(Event, Timestamp), (print(Event), nl)),
    nl,
    ansi_format([fg(green)], "Fluents that hold at time ~w~n", [Date]),
    forall(holdsAt(Fluent, Timestamp), (print(Fluent), nl)),
    nl.

?- print_narrative.
