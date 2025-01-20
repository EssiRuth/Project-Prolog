%phr(R) --> verbe(P), exp(A), prep(P), exp(B), {calcul(P, A, B, R)}.
%verbe(P) --> [Mot] , {lexique(Mot,verbe,P), P=Mot}.
%prep(P) --> [Mot] , {lexique(Mot,P), P=Mot}.
%exp(X) --> [N], {number(N), X is N }.
%lexique(additionner,verbe,et).
%lexique(soustraire,verbe,de).
%lexique(multiplier,verbe,par).
%lexique(diviser,verbe,par).
%lexique(et,prep).
%lexique(de,prep).
%lexique(par,prep).

calcul(additionner, X, Y, R):- R is X+Y.
calcul(soustraire, X, Y, R):- R is X-Y.
calcul(multiplier, X, Y, R) :- R is X * Y.
calcul(diviser, X, Y, R) :- Y =\= 0, R is X / Y.

phr --> verbe(P,O), exp(X), prep(P), exp(Y).
verbe(P,Mot) --> [Mot], {lexique(Mot,verbe, P)}.
prep(P) --> [Mot] , {lexique(Mot,P), P=Mot}.
exp(X) --> [N], {number(N), X is N}.
lexique(additionner,verbe,et).
lexique(soustraire,verbe,de).
lexique(multiplier,verbe,par).
lexique(diviser,verbe,par).
lexique(et,prep).
lexique(de,prep).
lexique(par,prep).




