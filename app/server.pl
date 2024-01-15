:- [cached_dec].

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).

server(Port) :- http_server(http_dispatch, [port(Port)]).
:- server(8000).

:- http_handler(root(_Path), http_reply_file('index.html', []), []).
:- http_handler(root('js/app.js'), http_reply_file('js/app.js', []), []).
:- http_handler(root('css/style.css'), http_reply_file('css/style.css', []), []).
:- http_handler(root(update), update_eec, []).

write_to_file(Rules, Narrative, Filename) :-
    Preamble = ':- initialiseDEC. :- retractall(happens(_,_)).',
    Postamble = ':- generate_narrative.',
    Filename = 'client_code.pl',
    open(Filename, write, OS),
    (
        write(OS, Preamble),
        write(OS, Rules),
        write(OS, Narrative),
        write(OS, Postamble),
        false
        ;
        close(OS)
    ).

update_eec(Request) :-
    % Process the HTTP request
    member(method(post), Request), !,
    http_read_data(Request, Data, []),
    Data = [rules=Rules, narrative=Narrative],
    % Write the client's code to a local server file and consult it
    write_to_file(Rules, Narrative, File),
    [File],
    format('Content-type: text/plain~n~n', []),
    [queries].
