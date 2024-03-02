%%% Rules

set_trust(interaction(A, B, C), A, B, C).
set_judgement(verify(A, B, C, D), A, B, C, D).

% Initial trust levels
happens(interaction(customer, winery, 1.0), 0).
happens(interaction(customer, transportation, 0.95), 0).

% If organic and non toxic, then also healthy
happens(verify(customer, true, (organic, non_toxic ==> healthy), 1.0), 0).

% Things that are healthy are also good
happens(verify(customer, true, (healthy ==> good), 1.0), 0).


%%% Narrative

% Winery claims that the bottle of wine is organic
happens(verify(winery, "audit1", organic, 1.0), 1001).

% % Claims that the bottle was not changed in an inorganic way
happens(verify(transportation, "audit2", non_toxic, 1.0), 1101).

