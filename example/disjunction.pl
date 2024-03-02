set_judgement(verify(A, B, C, D), A, B, C, D).


% Create the implicative claim
happens(verify(customer, true, (a ; (b, (c ; e)) ; d ==> f), 0.95), 0).

% Satisfy a
happens(verify(customer, t1, a, 0.9), 1).

% Satisfy d
happens(verify(customer, t2, d, 0.8), 2).

% Satisfy (b, (c ; e))
happens(verify(customer, t3, b, 0.7), 3).
happens(verify(customer, t4, c, 0.8), 3).
happens(verify(customer, t5, e, 0.5), 4).
