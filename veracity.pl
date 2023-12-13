:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).


%%% Rules

% Initial trust levels
initially(trust(customer, winery)=1.0).
initially(trust(customer2, customer)=0.9).
initially(trust(customer2, winery)=0.95).
initially(trust(customer, organic_transportation)=0.97).
initially(trust(customer, random_transportation)=0.4).
initially(trust(customer, retailer)=0.95).

% If Entity verifies a Claim, it creates a judgement that some Evidence witnesses the Claim
% Evidence has to be atomic; it is not further analysable in the veracity logic (though it may still be a complex prolog structure)
% To supply complex Evidence involving multiple judgements, use 'implies'
initiates(verify(Entity, Evidence, Claim, Confidence), judgement(Entity, [Evidence], Claim)=Confidence, _T).

% If A judges that O is organic at T_Prev, and A judges at T that O has not changed since T_Prev, then A should also judge that O is organic at T
implies([judgement(A, _, claim(O, "organic", T_Prev))=_, judgement(A, _, claim(O, no_change(T_Prev), T))=_], judgement(A, _, claim(O, "organic", T))=_, T).

initiates(change_trust(A, B, Trust), trust(A, B)=Trust, _T).

%%% Narrative

% Handles the bottle
% happens(handle(winery, bottle_1), 1000).
% Winery claims that the bottle of wine is organic
happens(verify(winery, "audit", claim(bottle_1, "organic", 1001), 1.0), 1001).

happens(query, 1002).

happens(change_trust(customer, winery, 0.9), 1003).

happens(query, 1004).
% % Transportation handles the bottle
% happens(handle(organic_transportation, bottle_1), 1100).
% % Claims that the bottle was not changed in an inorganic way
% happens(witness(organic_transportation, claim(bottle_1, "organic"), 1.0), 1101).

% % Retailer handles the bottle
% happens(handle(retailer, bottle_1), 1200).
% % Claims that the bottle was not changed in an inorganic way
% happens(witness(retailer, claim(bottle_1, "organic"), 1.0), 1201).


% % What fluents hold at T=1500?
% happens(query, 1500).


% % The winery handles the second bottle
% % happens(handle(winery, bottle_2), 2000).
% % Winery claims that the second bottle of wine is organic
% happens(witness(winery, claim(bottle_2, "organic"), 1.0), 2001).

% % Transportation handles the bottle
% happens(handle(random_transportation, bottle_2), 2100).
% % Claims that the bottle was not changed in an inorganic way
% happens(witness(random_transportation, claim(bottle_2, "organic"), 1.0), 2101).

% % Retailer handles the bottle
% happens(handle(retailer, bottle_2), 2200).
% % Claims that the bottle was not changed in an inorganic way
% happens(witness(retailer, claim(bottle_2, "organic"), 1.0), 2201).

% % What fluents hold at T=2500?
% happens(query, 2500).
