%%% Rules

set_trust(interaction(A, B, C), A, B, C).
set_judgement(verify(A, B, C, D), A, B, C, D).

%%% Narrative

happens(interaction(retailer, vineyard, 0.97), 0).
happens(interaction(retailer, winery, 0.98), 0).
happens(interaction(customer, retailer, 0.9), 0).
happens(interaction(customer, winery, 0.95), 0).


happens(verify(vineyard, testing, non_toxic, 1.0), 1).

happens(verify(winery, audit, organic, 0.95), 2).
