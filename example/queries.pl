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
    forall((fluent(Fluent, Future), Fluent \= (judgement(_, _, _)=_)), (print(Fluent), nl)),
    forall(fluent(judgement(A, proof(Success, _, _), C)=K, Future), (
            print(judgement(A,'[...]', C)=K),
            % Include whether or not the proof was successfully verified by Coq
            write('   : '), print(Success), nl
        )),
    nl, !.

?- print_narrative.


% retractall(happens(_,_)), ['logic/cached_dec', '../example/supply'].
% retractall(happens(_,_)), ['logic/cached_dec', '../example/supply_simple'].
% retractall(happens(_,_)), ['logic/cached_dec', '../example/disjunction'].
% ['../example/queries'].
