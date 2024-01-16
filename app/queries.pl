:- use_module(library(ansi_term)).

construct_json_narrative([Current | Remaining]) :-
    % Each entry consists of the timestamp, events which occurred, and the fluents which hold just after.
    Current = _{timestamp : Timestamp, events : Events, fluents : Fluents},
    adjacent_timestamps(Timestamp, Future),
    % Collect the events and fluents, moving the narrative along in between
    findall(JsonEvent, (happens(Event, Timestamp), jsonify(Event, JsonEvent)), Events),
    tick,
    findall(JsonFluent, (holdsAt(Fluent, Future), jsonify(Fluent, JsonFluent)), Fluents),
    % Recurse through the narrative until the length is less than one
    (
        narrative(Narrative), length(Narrative, Length), Length >= 1,
        construct_json_narrative(Remaining)
    ;
        Remaining = []
    ).

% Parse the standard Prolog functor syntax into actual JSON so that it can be manipulated more easily on client-side.
jsonify(In, Out) :-
    % Check for the =/2 Type, since it is used for multi-valued fluents
    (
        In = (Structure = Value)
        ;
        In = (Structure)
    ),
    % Unify the functor and args with a list, then recursively jsonify the raw arguments
    Structure =.. [Type | UnprocessedArgs],
    findall(ProcessedArg, (member(UnprocessedArg, UnprocessedArgs), jsonify(UnprocessedArg, ProcessedArg)), Args),
    % Unify Out with valid JSON
    (
        ((Args \= [], nonvar(Value)) -> Out = _{type : Type, args : Args, value : Value})
        ; (Args \= [] -> Out = _{type : Type, args : Args})
        ; (nonvar(Value) -> Out = _{type : Type, value : Value})
    ), !.


print_narrative :-
    ansi_format([fg(cyan)], "Printing entire narrative:~n~n", []),
    initialiseDEC,
    repeat,
    print_dec_state, nl,
    narrative(Narrative), length(Narrative, Length), Length =< 1.

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
    nl.

?- print_narrative.
