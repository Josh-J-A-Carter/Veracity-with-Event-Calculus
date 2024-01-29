%%% Rules

% Initial trust levels
initially(trust(retailer, vineyard)=0.97).
initially(trust(retailer, winery)=0.98).
initially(trust(customer, retailer)=0.9).
initially(trust(customer, winery)=0.95).

% Verification of a claim creates a judgement in the Veracity Logic
initiates(verify(Entity, Evidence, Claim, Confidence), judgement(Entity, [Evidence], Claim)=Confidence, _T).



%%% Narrative

happens(verify(vineyard, testing, non_toxic, 1.0), 1).

happens(verify(winery, audit, organic, 0.95), 2).

