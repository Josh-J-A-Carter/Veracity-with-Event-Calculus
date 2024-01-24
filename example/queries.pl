:- use_module(library(ansi_term)).

print_narrative :-
    ansi_format([fg(cyan)], "Printing entire narrative:~n~n", []),
    initialiseDEC,
    repeat,
    print_dec_state, nl,
    narrative(Narrative), length(Narrative, Length), Length < 1.

print_dec_state :-
    adjacent_timestamps(Timestamp, Future),
    format_time(atom(Date), '%d/%m/%Y, %H:%M', Timestamp),
    ansi_format([fg(green)], "Events that occurred at time ~w~n", [Date]),
    forall(happens(Event, Timestamp), (print(Event), nl)),
    tick,
    nl,nl,
    ansi_format([fg(green)], "Fluents that hold just after ~w~n", [Date]),
    forall((holdsAt(Fluent, Future), Fluent \= (judgement(_, _, _)=_)), (print(Fluent), nl)),
    forall(holdsAt(judgement(J, _E, C)=K, Future), (print(judgement(J,'[...]', C)=K), nl)),
    nl, !.

?- print_narrative.


% retractall(happens(_,_)), [app/cached_dec, example/production_process].
% [example/queries].