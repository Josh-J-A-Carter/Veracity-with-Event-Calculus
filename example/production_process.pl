%%% Rules

% Initial trust levels
initially(trust(customer, winery)=0.98).
initially(trust(customer, vineyard)=0.97).
initially(trust(customer, water_supplier)=0.9).
initially(judgement(customer, [axiom], [claim(Grapes, "organic", T1), claim(Water, "organic", T2), claim(combined(Grapes, Water, Wine), T3), constraint(T1 =< T3, T2 =< T3)] ==> claim(Wine, "organic", T3))=1.0).

initiates(verify(Entity, Evidence, Claim, Confidence), judgement(Entity, [Evidence], Claim)=Confidence, _T).
initiates(change_trust(A, B, Trust), trust(A, B)=Trust, _T).


%%% Narrative

happens(verify(vineyard, "audit_1", claim(grapes, "organic", 1001), 1.0), 1001).

happens(verify(water_supplier, "audit_2", claim(water, "organic", 2001), 1.0), 2001).

happens(verify(winery, "audit_3", claim(combined(grapes, water, wine), 4001), 1.0), 4001).

happens(change_trust(customer, winery, 0.95), 5001).
