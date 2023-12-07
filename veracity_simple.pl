:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).


%%% Rules
initially(trust(e1, e2)=0.5).
initially(trust(e3, e2)=0.75).
initially(trust(e3, e1)=0.2).

initiates(give_trust(Trustor, Trustee, Trust), trust(Trustor, Trustee)=Trust, _T).
initiates(event, belief(e2, e2, statement(wine, "organic"), T)=1.0, T).
initiates(event2, belief(e2, e2, statement(wine, "organic"), 1)=0.5, _T).
initiates(event3, belief(e2, e2, statement(wine, "organic"), T)=0.8, T).

violates(handle(_Handler, Object), expectation(_Entity, statement(Object, _Quality)), _T).

%%% Narrative

happens(event, 1).
happens(event2, 2).
happens(event3, 2).
happens(handle(e1, wine), 3).
happens(event, 4).
happens(give_trust(e3, e2, 0.5), 5).
happens(give_trust(e3, e2, 0.5), 6).

happens(query, 7).

