:- [cached_dec].

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

% Make sure that compilation errors/warnings are reported from loaded files
:- multifile user:message_hook/3.
user:message_hook(load_file_errors(_File, Errors, Warnings), _Level, _Lines) :-
    (Errors > 0 ; Warnings > 0), throw(error(syntax_error('no information'), Errors > 0)).

server(Port) :- http_server(http_dispatch, [port(Port)]).
:- server(8000).

:- http_handler(root(_Path), http_reply_file('index.html', []), []).
:- http_handler(root('js/app.js'), http_reply_file('js/app.js', []), []).
:- http_handler(root('js/cytoscape.js'), http_reply_file('js/cytoscape.js', []), []).
:- http_handler(root('js/cytoscape-klay.js'), http_reply_file('js/cytoscape-klay.js', []), []).
:- http_handler(root('js/klay.js'), http_reply_file('js/klay.js', []), []).
:- http_handler(root('css/style.css'), http_reply_file('css/style.css', []), []).
:- http_handler(root('update_dec'), update_dec, []).

update_dec(Request) :-
    % Process the HTTP request, parsing the body JSON, and extracting relevant key-value pairs
    member(method(post), Request), !,
    http_read_data(Request, Data, []),
    atom_json_term(Data, InJSON, []),
    InJSON = json(Elements), member(code=Code, Elements),
    % Write the client's code to a file, consult it, and return the output as JSON
    catch(
        (
            write_to_file(Code, File),
            retractall(happens(_,_)), [File], initialiseDEC,
            construct_json_narrative(Timeline),
            OutJSON = (_{success : true, timeline : Timeline}),
            format('Content-type: application/json~n~n', []),
            json_write(current_output, OutJSON, [true(true)])
        ),
        error(_, IsError),
        (
            format('Content-type: application/json~n~n', []),
            % Can be an error OR a warning, and we want to preserve this information
            (
                IsError -> ErrorType = 'syntax_error'
                ; ErrorType = 'warning'
            ),
            json_write(current_output, _{ success : false, error_type : ErrorType }, [false(false), true(true)])
        )
    ).

% Write the client's code to a local server file
write_to_file(Code, Filename) :-
    Filename = 'client_code.pl',
    open(Filename, write, OS),
    (
        write(OS, Code),
        false
        ;
        close(OS)
    ).

% Generate the output from the narrative as a JSONifiable dictionary
construct_json_narrative([Current | Remaining]) :-
    % Each entry consists of the timestamp, events which occurred, and the fluents which hold just after.
    Current = _{timestamp : Timestamp, events : Events, fluents : Fluents},
    adjacent_timestamps(Timestamp, Future),
    % Collect the events and fluents, moving the narrative along in between
    findall(JsonEvent, (happens(Event, Timestamp), jsonify(Event, JsonEvent)), Events),
    tick,
    findall(JsonFluent, (fluent(Fluent, Future), jsonify(Fluent, JsonFluent)), Fluents),
    % Recurse through the narrative until the length is less than one
    !, (
        narrative(Narrative), length(Narrative, Length), Length >= 1,
        construct_json_narrative(Remaining)
    ;
        Remaining = []
    ).

% Deal with variables separately to avoid infinite recursion
jsonify(Variable, Atom) :- var(Variable), term_to_atom(Variable, Atom), !.

% Parse compound terms into dictionaries so that they can be turned into JSON
% First, treat the |/2 functor specially so that lists are properly processed
jsonify([Head | Tail], [JsonHead | JsonTail]) :-
    jsonify(Head, JsonHead),
    jsonify(Tail, JsonTail), !.
jsonify([], []) :- !.

% Treat implications separately
jsonify(Conditions ==> Consequence, JsonImplication) :- 
    jsonify(Conditions, JsonConditions), jsonify(Consequence, JsonConsequence),
    JsonImplication = _{type : implies, args : [JsonConditions, JsonConsequence]}, !.

jsonify(In, Out) :-
    % Check for the =/2 functor, since it is used with multi-valued fluents
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
        (((Args \= []), nonvar(Value)) -> Out = _{type : Type, args : Args, value : Value})
        ; ((Args \= []) -> Out = _{type : Type, args : Args})
        ; (nonvar(Value) -> Out = _{type : Type, value : Value})
        ; (Out = Type)
    ), !.
