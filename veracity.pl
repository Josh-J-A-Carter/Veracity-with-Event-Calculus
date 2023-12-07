:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).


%%% Rules

% Initial trust levels
initially(trust(customer, winery)=1.0).
initially(trust(customer, organic_transportation)=0.97).
initially(trust(customer, random_transportation)=0.4).
initially(trust(customer, retailer)=0.95).

% If E1 witnesses some Quality about Object, and E2 trusts E1, then E2 now believes this claim.
initiates(witness(Entity, Claim, Confidence), belief(Entity, Entity, Claim, T)=Confidence, T).

% If E1 handles Object, then any entity E2's belief in any claim about Object is terminated.
violates(handle(_Handler, Object), expectation(_Entity, statement(Object, _Quality)), _T).



%%% Narrative

% Winery claims that the bottle of wine is organic
happens(witness(winery, claim(bottle_1, "organic"), 1.0), 1000).
% Then handles the bottle
happens(handle(winery, bottle_1), 1000).

% Claims that the bottle was not changed in an inorganic way
happens(witness(organic_transportation, claim(bottle_1, "organic"), 1.0), 1100).
% Transportation handles the bottle
happens(handle(organic_transportation, bottle_1), 1100).

% Claims that the bottle was not changed in an inorganic way
happens(witness(retailer, claim(bottle_1, "organic"), 1.0), 1200).
% Retailer handles the bottle
happens(handle(retailer, bottle_1), 1200).

% What fluents hold at T=1500?
happens(query, 1500).


% Winery claims that the second bottle of wine is organic
happens(witness(winery, claim(bottle_2, "organic"), 1.0), 2000).
% The winery handles the second bottle
happens(handle(winery, bottle_2), 2000).

% Claims that the bottle was not changed in an inorganic way
happens(witness(random_transportation, claim(bottle_2, "organic"), 1.0), 2100).
% Transportation handles the bottle
happens(handle(random_transportation, bottle_2), 2100).

% Claims that the bottle was not changed in an inorganic way
happens(witness(retailer, claim(bottle_2, "organic"), 1.0), 2200).
% Retailer handles the bottle
happens(handle(retailer, bottle_2), 2200).

% What fluents hold at T=2500?
happens(query, 2500).
