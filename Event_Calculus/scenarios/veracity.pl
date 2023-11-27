:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).


% Rules
initially(trust(person1, authority)=0.9).
initially(trust(person2, person1)=0.9).
initially(trust(person3, person2)=0.8).

initiates(generate_judgement(Entity, Claim, Confidence), judgement(Entity, Claim)=Confidence, _T).
initiates(update_trust(Trustor, Trustee, New_Confidence), trust(Trustor, Trustee)=New_Confidence, _T).

terminates(retract_judgement(Entity, Claim), judgement(Entity, Claim)=_Confidence, _T).

holdsIf(judgement(E1, Claim)=Secondary_Confidence, T) :-
    holdsAt(trust(E1, E2)=Trust, T),
    holdsAt(judgement(E2, Claim)=Confidence, T),
    Secondary_Confidence is Trust * Confidence.


% Narrative
happens(generate_judgement(authority, statement, 1.0), 0).
happens(update_trust(person1, authority, 0.7), 1).
happens(update_trust(person3, person2, 0.9), 2).
happens(retract_judgement(authority, statement), 3).

