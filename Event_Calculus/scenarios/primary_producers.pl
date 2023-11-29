:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).
:- use_module(library(date)).


% Applicant begins the initial application
initiates(begin_application(Entity, Certifier), application(Entity, Certifier)=pending_first_audit, T) :-
    \+ holdsAt(application(Entity, Certifier)=_Status, T).

% Pass the initial audit
causes(pass_audit(Certifier, Entity, Start_Date, Expiry_Date), T, certify(Certifier, Entity, Expiry_Date, c0), Start_Date) :-
    holdsAt(application(Entity, Certifier)=pending_first_audit, T).
initiates(pass_audit(Certifier, Entity, _Start_Date, _Expiry_Date), application(Entity, Certifier)=pending_second_audit, T) :-
    holdsAt(application(Entity, Certifier)=pending_first_audit, T).

% Pass the second audit
causes(pass_audit(Certifier, Entity, Start_Date, Expiry_Date), T, certify(Certifier, Entity, Expiry_Date, c1), Start_Date) :-
    holdsAt(application(Entity, Certifier)=pending_second_audit, T).
initiates(pass_audit(Certifier, Entity, _Start_Date, _Expiry_Date), application(Entity, Certifier)=pending_third_audit, T) :-
    holdsAt(application(Entity, Certifier)=pending_second_audit, T).

% Pass the third audit
causes(pass_audit(Certifier, Entity, Start_Date, Expiry_Date), T, certify(Certifier, Entity, Expiry_Date, c2), Start_Date) :-
    holdsAt(application(Entity, Certifier)=pending_third_audit, T).
initiates(pass_audit(Certifier, Entity, _Start_Date, _Expiry_Date), application(Entity, Certifier)=pending_fourth_audit, T) :-
    holdsAt(application(Entity, Certifier)=pending_third_audit, T).

% Pass the fourth audit
causes(pass_audit(Certifier, Entity, Start_Date, Expiry_Date), T, certify(Certifier, Entity, Expiry_Date, "BioGro Certification"), Start_Date) :-
    holdsAt(application(Entity, Certifier)=pending_fourth_audit, T).
terminates(pass_audit(Certifier, Entity, _Start_Date, _Expiry_Date), application(Entity, Certifier)=pending_fourth_audit, _T).

% Renewal of certificate
causes(pass_audit(Certifier, Entity, Start_Date, Expiry_Date), T, certify(Certifier, Entity, Expiry_Date, "BioGro Certification"), Start_Date) :-
    holdsAt(certificate(Entity, Certifier)="BioGro Certification", T).

% Certification and expiration
initiates(certify(Certifier, Entity, _Expiry_Date, Certificate), certificate(Entity, Certifier)=Certificate, _T).
causes(certify(Certifier, Entity, Expiry_Date, _Certificate), _T, expire(Certifier, Entity), Expiry_Date).
terminates(expire(Certifier, Entity), certificate(Entity, Certifier)=Status, T) :-
    \+ happens(certify(Certifier, Entity, _Expiry_Date, Status), T).


% The narrative
happens(begin_application(producer, certifier), 1000).
happens(pass_audit(certifier, producer, 1200, 2200), 1200).
happens(pass_audit(certifier, producer, 2200, 3200), 2000).
happens(pass_audit(certifier, producer, 3200, 4200), 3000).
happens(pass_audit(certifier, producer, 4200, 5200), 4000).
happens(pass_audit(certifier, producer, 5200, 6200), 5100).

happens(query, 6201).
