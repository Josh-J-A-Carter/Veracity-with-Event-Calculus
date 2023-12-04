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
initiates(witness(E1, claim(Object, Quality), Confidence), belief(E2, claim(Object, Quality))=New_Confidence, T) :-
    holdsAt(trust(E2, E1)=Trust, T),
    (
        % How sure were we of this claim before?
        holdsAt(belief(E2, claim(Object, Quality))=Previous_Confidence, T),
        New_Confidence is Previous_Confidence * (Confidence * Trust), !
        ;
        % If we had no belief before, then we can ignore previous confidence
        New_Confidence is Confidence * Trust, !
    ).

% If E1 handles Object, then any entity E2's belief in any claim about Object is terminated.
terminates(handle(_E1, Object), belief(_E2, claim(Object, _Quality))=_Confidence, _T).



%%% Narrative

% The winery first handles the bottle
happens(handle(winery, bottle_1), 1000).
% Then it claims that the bottle of wine is organic
% (i.e. it claims that the supplied materials used to make the wine were organic)
happens(witness(winery, claim(bottle_1, "organic"), 1.0), 1000).

% Transportation handles the bottle
happens(handle(organic_transportation, bottle_1), 1100).
% Then it claims that the bottle was not changed in an inorganic way
happens(witness(organic_transportation, claim(bottle_1, "organic"), 1.0), 1100).

% Retailer handles the bottle
happens(handle(retailer, bottle_1), 1200).
% Then it claims that the bottle was not changed in an inorganic way
happens(witness(retailer, claim(bottle_1, "organic"), 1.0), 1200).

% What fluents hold at T=1500?
happens(query, 1500).



% The winery first handles the second bottle
happens(handle(winery, bottle_2), 2000).
% Then it claims that the bottle of wine is organic
% (i.e. it claims that the supplied materials used to make the wine were organic)
happens(witness(winery, claim(bottle_2, "organic"), 1.0), 2000).

% Transportation handles the bottle
happens(handle(random_transportation, bottle_2), 2100).
% Then it claims that the bottle was not changed in an inorganic way
happens(witness(random_transportation, claim(bottle_2, "organic"), 1.0), 2100).

% Retailer handles the bottle
happens(handle(retailer, bottle_2), 2200).
% Then it claims that the bottle was not changed in an inorganic way
happens(witness(retailer, claim(bottle_2, "organic"), 1.0), 2200).

% What fluents hold at T=2500?
happens(query, 2500).
