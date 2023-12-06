:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).


%%% Rules
initially(trust(e1, e2)=0.5).
initially(trust(e3, e2)=0.75).
initially(trust(e3, e1)=0.2).

initiates(event, belief(e2, e2, eggs, T)=1.0, T).

%%% Narrative

happens(event, 1).

