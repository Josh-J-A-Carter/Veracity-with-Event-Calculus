%%% Rules

set_trust(interaction(A, B, C), A, B, C).
set_judgement(verify(A, B, C, D), A, B, C, D).

% Initial trust levels
happens(interaction(customer, winery, 1.0), 0).
happens(interaction(customer2, customer, 0.9), 0).
happens(interaction(customer2, winery, 0.95), 0).
happens(interaction(customer, organic_transportation, 0.97), 0).
happens(interaction(customer, retailer, 0.95), 0).

% If (O is verified as organic at T1) and (O is verified as not having changed since T1),
% then (O is verified as organic at T2) - provided T1 < T2
happens(verify(customer, true, (claim(O, "organic", T1), claim(O, no_change(T1), T2), {T1 < T2} ==> claim(O, "organic", T2)), 1.0), 0).
happens(verify(customer2, true, (claim(O, "organic", T1), claim(O, no_change(T1), T2), {T1 < T2} ==> claim(O, "organic", T2)), 1.0), 0).


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


happens(verify(customer, true, (claim(O, "organic", T1), claim(O, no_change(T1), T2), {T1 < T2} ==> claim(O, "organic", T2)), 0), 1301).


happens(verify(customer2, true, (claim(O, "organic", T1), claim(O, no_change(T1), T2), {T1 < T2} ==> claim(O, "organic", T2)), 1.0), 1302).



