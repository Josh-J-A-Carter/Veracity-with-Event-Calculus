%%% Rules

% Initial trust levels
initially(trust(customer, winery)=1.0).
initially(trust(customer2, customer)=0.9).
initially(trust(customer2, winery)=0.95).
initially(trust(customer, organic_transportation)=0.97).
initially(trust(organic_transportation, random_transportation)=0.2).
initially(trust(customer, retailer)=0.95).

% If Entity verifies a Claim, it creates a judgement that some Evidence verifies the Claim
% Evidence has to be atomic; it is not further analysable in the veracity logic (though it may still be a complex prolog structure)
% To supply complex Evidence involving multiple judgements, use 'implies'
initiates(verify(Entity, Evidence, Claim, Confidence), judgement(Entity, [Evidence], Claim)=Confidence, _T).

% If (O is verified as organic at T1) and (O is verified as not having changed since T1),
% then (O is verified as organic at T2) - provided T1 < T2
implies([claim(O, "organic", T1), claim(O, no_change(T1), T2)] ==> claim(O, "organic", T2), _T) :- T1 < T2.

initiates(change_trust(A, B, Trust), trust(A, B)=Trust, _T).


%%% Narrative

% Handles the bottle
happens(handle(winery, bottle_1), 1000).
% Winery claims that the bottle of wine is organic
happens(verify(winery, "audit", claim(bottle_1, "organic", 1001), 1.0), 1001).

% % Transportation handles the bottle
happens(handle(organic_transportation, bottle_1), 1100).
% % Claims that the bottle was not changed in an inorganic way
happens(verify(organic_transportation, "audit", claim(bottle_1, no_change(1001), 1101), 1.0), 1101).

% % Retailer handles the bottle
happens(handle(retailer, bottle_1), 1200).
% % Claims that the bottle was not changed in an inorganic way
happens(verify(retailer, "audit", claim(bottle_1, no_change(1101), 1201), 1.0), 1201).


% % What fluents hold at T=1500?
happens(query, 1500).