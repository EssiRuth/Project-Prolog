infPrem([_]).
infPrem([]).
infPrem([X, Y|R]):- Y=<X, infPrem([X|R]).

motard:- between(2, 8, A), between(10000, 99999, B), Z is A*B, Z>0, U is Z mod 10, D is (Z//10) mod 10, C is (Z//100) mod 10, M is (Z//1000) mod 10, N is (Z//10000) mod 10, Z is U + D*10 + C*100 + M*1000 + N*10000, B is  U*10000 + D*1000 + C*100 + M*10 + N, Z\=B, B\= Z, writef("le nombre de copine est %t et le prix de la moto est %t\n", [A, B]).

premier(N):-R is integer(sqrt(N)), between(2, R, X), N mod X =:=0, !, fail.
premier(_).

exercice4(A, B) :- between(1, 9999, A), between(1, 9999, B), premier(A), premier(B),write(" "), write(B), nl, (A // 1000) mod 10=\= (B // 1000) mod 10, abs(A-B) =:= 8.

mul([], _, []).
mul([X|R], C, [Z|L]):- Z is X*C, mul(R, C, L).

permut([], []).
permut(L, [X|R]):- append(P, [X|S], L), append(P, S, A), permut(A, R).

 per([], []).
per([X|R], [Y|L]):- X\==Y, per(R, L).

prix(pomme, 7).
prix(melon, 13).
prix(banane, 8).
total([], 0).
total([[X, Y]|R], T) :- prix(X, A), T1 is Y*A, total(R, T2), T is T1+T2.


lettres([],  []).
lettres([X|R], L):- atom_chars(X, L1), append(L1, A, L), lettres(R, A).
 r(0,0).
 r(X,Y):- p(X), q(Y),!.
 r(Y,X):- p(Y), q(Y), X=3, Y<X.
 r(5,5).
 p(1).
 p(2).
 q(2).
 q(3).

a(X):-b(X).
a(3).
a(Y):-b(X), Y is X*X.
b(X):-c(X), X>0.
b(X):-c(X), X<4.
c(3).
c(6).

%X=3, X=3, X=9
%b(X) ,X=3, X=6, X=3
%a(X), X=3, X=6, X=3, X=3, X=9, X=36, X=9

aime(marie,jean).
aime(marie,paul).
aime(jean,marie).
amoure(X, Y):- aime(X, Y), not((aime(X, S), S\=Y)).
amourf(X, Y):- amoure(X,Y), amoure(Y, X).


decalerG(L,0,L).
decalerG([], _,[]).
decalerG(X, N, R):-  append(Y,Z, X), length(Y, N), append(Z, Y, R).
decalerD(X, N, R):-  append(Y, Z, X), length(Z, N), append(Z,Y,R).

nbOcc([], 0).
contient([], _).
contient([X|Xs], [X|Ys]) :- contient(Xs, Ys),!.
contient(Xs, [_|Ys]) :- contient(Xs, Ys),!.

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

%Exercice2
v(A) :- s(A,A).
v(C) :- s(C,D), C<D.
s(A,B):- between(1,3,A),!,between(1,3,B).
%X=1, X=2, X=3, X=1, X=1, X=2
%X=1, X=1, X=1

%Exercice3
conjugue(Verbe):-
    atom_concat(R, 'er', Verbe),
    conjuguer(R).

conjuguer(R):-
    write('je'), write(' '), write(R), write('e'), nl,
    write('tu'), write(' '), write(R), write('es'), nl,
    write('il'), write(' '), write(R), write('e'), nl,
    write('nous'), write(' '), write(R), write('ons'), nl,
    write('vous'), write(' '), write(R), write('ez'), nl,
    write('ils'), write(' '), write(R), write('ent').

%Session 1
%Exercice1
%Additionner un et deux

phr --> verbe(CV,P), exp, prep(P), exp, { verb(CV)}.
verbe(Mot,P) --> [Mot] , {lexique1(Mot, P)}.
prep(P) --> [Mot] , {lexique(Mot,prep), P=Mot}.
exp --> chiffre.
chiffre --> ['un'].
chiffre -->['deux'].
chiffre -->['trois'].
chiffre -->['quatre'].
chiffre -->['cinq'].
verb(additionner).
verb(soustraire).
verb(multiplier).
lexique1(additionner,et).
lexique1(soustraire, de).
lexique1(multiplier, par).

lexique(et,prep).
lexique(de,prep).
lexique(par,prep).

animal(alligator).

animal(caribours).
animal(caribour).
animal(ours).

mutant(L):-animal(X),animal(Y),atom_chars(X,LX),atom_chars(Y,LY),
 append(LX1,LX2,LX), not(LX1=[]),not(LX2=[]),
 append(LX2,LY1,LY),append(LX,LY1,L1),atom_chars(L,L1),not(animal(L)).
