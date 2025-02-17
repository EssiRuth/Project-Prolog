
affiche([A,B]):- not(member(A, [+,-,*,**,***,/])),
not(number(B)),write('('), write(A), write('('), write(B), write(')'),
write(')').

affiche([A, B, C]):- member(A, [+, -, *, **, /]),not(is_list(B)), not(is_list(C)),((not(number(B)), number(C));(not(number(C)), number(B))), write('('), write(B), write(A), write(C), write(')').

affiche([A,B,C]):- is_list(B),is_list(C), write('('), affiche(B), write(A), affiche(C), write(')').

affiche([A,B,C]):- member(A, [+, -, *, **, /]), is_list(C), number(B), write('('), write(B), write(A), affiche(C), write(')').


deriv([A, B, C], [A, D, E]):- not(number(A)), not(number(B)), number(C), D=1, E=0.


calcul(A, A):- number(A).
calcul([+, A, B], R):- calcul(A, D), calcul(B, P), R is  D+P.
calcul([-, A, B], R):- calcul(A, D),  calcul(B, P), R is  D-P.
calcul([*, A, B], R):- calcul(A, D),  calcul(B, P), R is  D*P.
calcul([/, A, B], R):- calcul(A, D),  calcul(B, P), R is  D/P.


%TP5
phr(R) --> verbe(P), exp(A), prep(P), exp(B), {calcul(P, A, B, R)}.
verbe(P) --> [Mot] , {lexique(Mot,verbe,P)}.
prep(P) --> [Mot] , {lexique(Mot,P,P)}.
exp(X) --> [N], {number(N), X is N }.
lexique(additionner,verbe,et).
lexique(soustraire,verbe,de).
lexique(multiplier,verbe,par).
lexique(diviser,verbe,par).
lexique(et,prep,et).
lexique(de,prep,de).
lexique(par,prep,par).

calcul(additionner, X, Y, R):- R is X+Y.
calcul(soustraire, X, Y, R):- R is X-Y.
calcul(multiplier, X, Y, R):- R is X*Y.
calcul(diviser, X, Y, R):- Y=\=0, R is X/Y.
