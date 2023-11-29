:- [cached_dec].
:- initialiseDEC.
:- retractall(happens(_,_)).
:- use_module(library(date)).

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


%%% Certification

% Accepting the application may entail certification commencing at some later date
causes(accept_application(Certifier, Entity, Start_Date, _Expiry_Date), _Acceptance_Date, certify(Certifier, Entity), Start_Date).
    % holdsAt(application(Entity, Certifier)=pending_audit, Acceptance_Date).
terminates(accept_application(Certifier, Entity, _Start_Date, _Expiry_Date), application(Entity, Certifier)=pending_audit, _T).
initiates(certify(Certifier, Entity), certificate(Entity, Certifier)=valid, _T).

% Certification causes expiry to occur after some time delay
causes(accept_application(Certifier, Entity, _Start_Date, Expiry_Date), _Acceptance_Date, expire(Certifier, Entity), Expiry_Date).
    % holdsAt(application(Entity, Certifier)=pending_audit, Acceptance_Date).
terminates(expire(Certifier, Entity), certificate(Entity, Certifier)=_Status, _T).

% Renewal of certificate, which starts a new application
initiates(request_renewal(Entity, Certifier), application(Entity, Certifier)=pending_payment, T) :-
    holdsAt(certificate(Entity, Certifier)=valid, T).

% The application could also be denied
terminates(deny_application(Certifier, Entity), application(Entity, Certifier)=_Status, _T).

%%% Penalties and corrective actions

initiates(suspend(Certifier, Entity), certificate(Entity, Certifier)=suspended, T) :-
    holdsAt(certificate(Entity, Certifier)=valid, T).

initiates(reinstate(Certifier, Entity), certificate(Entity, Certifier)=valid, T) :-
    holdsAt(certificate(Entity, Certifier)=Status, T), Status \= valid.

terminates(revoke(Certifier, Entity), certificate(Entity, Certifier)=_Status, _T).



% The narrative
happens(begin_application(producer, certifier), 1586901600).                                    % 15/04/2020, 10.00
happens(pay_fee(producer, certifier), 1586903400).                                              % 15/04/2020, 10.30
happens(provide_documentation(producer, certifier), 1586907000).                                % 15/04/2020, 11.30
happens(require_documentation(certifier, producer), 1587607200).                                % 23/04/2020, 14.00
happens(provide_documentation(producer, certifier), 1587776400).                                % 25/04/2020, 13.00
happens(accept_documentation(certifier, producer), 1588453200).                                 % 03/05/2020, 09.00
happens(accept_application(certifier, producer, 1592449200, 1623931200), 1592449200).           % 18/06/2020, 15.00 - 18/06/2021, 00.00
happens(suspend(certifier, producer), 1604979000).                                              % 10/11/2020, 16.30
happens(reinstate(certifier, producer), 1605132000).                                            % 12/11/2020, 11.00
happens(query, 1618012800).                                                                     % 10/04/2021, 00.00
happens(query, 1633651200).                                                                     % 08/10/2021, 00.00
