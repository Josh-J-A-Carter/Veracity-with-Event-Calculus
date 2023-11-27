:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).



%%% Initial application

% Applicant begins the initial application
initiates(begin_application(Entity, Certifier), application(Entity, Certifier)=pending_payment, T) :-
    \+ holdsAt(application(Entity, Certifier)=_Status, T).

% Applicant pays the assessment fee
initiates(pay_fee(Entity, Certifier), application(Entity, Certifier)=pending_documentation, T) :-
    holdsAt(application(Entity, Certifier)=pending_payment, T).

% Applicant provides documentation
initiates(provide_documentation(Entity, Certifier), application(Entity, Certifier)=pending_assessment, T) :-
    holdsAt(application(Entity, Certifier)=pending_documentation, T).


%%% Assessment

% Require more documentation
initiates(require_documentation(Certifier, Entity), application(Entity, Certifier)=pending_documentation, T) :-
    holdsAt(application(Entity, Certifier)=pending_assessment, T).

% Documentation is sufficient; now require an audit
initiates(accept_documentation(Certifier, Entity), application(Entity, Certifier)=pending_audit, T) :-
    holdsAt(application(Entity, Certifier)=pending_assessment, T).

% If the audit is passed, terminate the existing application.
% Note: _Start_Date is the date that the certification takes effect. This is particularly important for renewals.
terminates(certify(Certifier, Entity, _Start_Date), application(Entity, Certifier)=_Status, T) :-
    holdsAt(application(Entity, Certifier)=pending_audit, T).


%%% Certification

% Entity is certified by Certifier at time T if it has a current certification, effective within 12 months of T,
% and Certifier has not suspended or revoked Entity's certification.
holdsIf(certificate(Entity, Certifier)=valid, T) :-
    happens(certify(Certifier, Entity, T_previous), _Date_of_Decision),
    T - T_previous =< 12,
    T >= T_previous,
    \+ (
        holdsAt(penalty(Entity, Certifier)=suspension, T)
        ;
        holdsAt(penalty(Entity, Certifier)=revocation, T)
    ).

% Entity's certificate from Certifier has expired if we cannot find a recent certify event
holdsIf(certificate(Entity, Certifier)=expired, T) :-
    happens(certify(Certifier, Entity, T_previous), _Date_of_Decision),
    T >= T_previous,
    \+ T - T_previous =< 12,
    \+ (
        holdsAt(penalty(Entity, Certifier)=suspension, T)
        ;
        holdsAt(penalty(Entity, Certifier)=revocation, T)
    ).

% Suspension of licence
holdsIf(certificate(Entity, Certifier)=suspended, T) :-
    holdsAt(penalty(Entity, Certifier)=suspension, T).

% Revocation of licence
holdsIf(certificate(Entity, Certifier)=revoked, T) :-
    holdsAt(penalty(Entity, Certifier)=revocation, T).


% Renewal of certificate, which starts a new application
initiates(request_renewal(Entity, Certifier), application(Entity, Certifier)=pending_payment, T) :-
    holdsAt(certificate(Entity, Certifier)=valid, T).


%%% Penalties and corrective actions

initiates(suspend(Certifier, Entity), penalty(Entity, Certifier)=suspension, T) :-
    holdsAt(certificate(Entity, Certifier)=_Status, T).

initiates(revoke(Certifier, Entity), penalty(Entity, Certifier)=revocation, T) :-
    holdsAt(certificate(Entity, Certifier)=_Status, T).

% Clears any existing penalties
terminates(excuse(Certifier, Entity), penalty(Entity, Certifier)=_Punishment, _T).



% The narrative
happens(begin_application(producer, certifier), 0).
happens(pay_fee(producer, certifier), 1).
happens(provide_documentation(producer, certifier), 2).
happens(require_documentation(certifier, producer), 3).
happens(provide_documentation(producer, certifier), 4).
happens(accept_documentation(certifier, producer), 5).
happens(certify(certifier, producer, 7), 6).
happens(revoke(certifier, producer), 9).
happens(excuse(certifier, producer), 11).
