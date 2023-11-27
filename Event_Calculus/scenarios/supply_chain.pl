:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).

initially(possessor(wine)=producer).

initiates(transfer(E1, E2, Object), possessor(Object)=E2, T) :-
    holdsAt(possessor(Object)=E1, T).

% The narrative
happens(transfer(producer, transport, wine), 1).
happens(transfer(transport, store, wine), 2).
happens(transfer(store, customer, wine), 3).


