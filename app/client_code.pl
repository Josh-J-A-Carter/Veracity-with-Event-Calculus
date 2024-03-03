%%% Rules

set_trust(interaction(A, B, C), A, B, C).
set_judgement(verify(A, B, C, D), A, B, C, D).

% If organic and non toxic, then also healthy
happens(verify(customer, true, (c(Object, organic), c(Object, non_toxic) ==> c(Object, healthy)), 1.0), 0).


%%% Narrative

% Initial trust levels
happens(interaction(customer, winery, 0.9), 0).
happens(interaction(customer, transportation, 0.95), 0).

% Winery claims that the bottle of wine is organic
happens(verify(winery, audit1, c(wine, organic), 1.0), 1).

% % Claims that the bottle was not changed in an inorganic way
happens(verify(transportation, audit2, c(wine, non_toxic), 1.0), 2).

