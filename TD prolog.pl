rectangle(a).
rectangle(b).
losange(b).
carre(X) :- rectangle(X),losange(X).

nbpairs([],0).
nbpairs([T|Q], N) :- T mod 2 =:= 0, nbpairs(Q, N1), N is N1+1.
nbpairs([T|Q], N) :- T mod 2 =:= 1, nbpairs(Q, N).

nom(80112, marie, tudor).
ue(80112, [math1, math2, info2, eco1, eco2, ueo]).
note(80112, math1, 13).
note(80112, math2, 9).
note(80112, info2, 11).

passe(X, Y, Z) :- nom(Num, X, Y), ue(Num, UEs), member(Z, UEs).
noteMath(X, Y, M):- passe(X, Y, math1), passe(X, Y, math2), nom(Z, X, Y), note(Z, math1, A), note(Z, math2, B), M is( A+B)/ 2.
mentionMath(X, Y, M):- noteMath(X, Y, Z), Z < 10, M = echec.
mentionMath(X, Y, M):- noteMath(X, Y, Z), Z> 10, Z < 12, M = passable.
mentionMath(X, Y, M):- noteMath(X, Y, Z), Z > 12, Z < 14, M = assezBien.
mentionMath(X, Y, M):- noteMath(X, Y, Z), Z> 14, Z < 16, M = bien.
mentionMath(X, Y, M):- noteMath(X, Y, Z), Z >= 16, M = tresBien.

factorielle(0, 1).
factorielle(N, R):- N > 0, Y is N-1, factorielle(Y, N1), R is N * N1.

a(X):-b(X),c(X).
a(X):-d(X),e(X).
b(1).
b(Z):-d(Z).
c(1).
c(2).
d(2).
e(2).

infPrem([_]).
infPrem([]).
infPrem([X, Y|L]):- Y=<X, infPrem([X|L]).

couleur(rouge).
couleur(vert).
couleur(bleu).
colorier(A,B,C):- couleur(A), couleur(B), couleur(C),
 A\==B, A\==C, B\==C.

reunis([], _, []).
reunis(_, [], []).
reunis([X|Xs], [Y|Ys], [[X,Y]|Result]) :- reunis(Xs, Ys, Result).

supprimer(_, [], []).
supprimer(X, [X|L], R):- !,supprimer(X, L, R).
supprimer(X,[A|L], [A|R]):- supprimer(X, L, R).

petitsgateaux(N) :- between(2, 100, N),  N mod 2 =:= 1, N mod 3 =:= 1, N mod 4 =:= 1, N mod 5 =:= 1,  N mod 6 =:= 1.

repete(_, 0, []).
repete(X, Y, [X|Z]):- Y>0, Y1 is Y-1, repete(X, Y1, Z).


decoderpaquets([], []).
decoderpaquets([[X, Y]|R], [Z|L]):- repete(Y, X, Z), decoderpaquets(R, L).

decoder([], []).
decoder([[X, Y]|R], L):- repete(Y, X, Z), decoder(R, L1), append(Z, L1, L).

coderpaquets([], []).
coderpaquets([A|R], [Z|L]):- [X|_]=A, length(A, Y), Z = [Y, X], coderpaquets(R, L).



eclate([], _, []).
eclate([X|Y], N, [A|R]):- repete(X, N, A), eclate(Y, N, R).

superAppend([], []).
superAppend([X|L], R):- superAppend(L, R1), append(X, R1, R ).

seulNbre(R):- between(2, 100, R), R mod 2 =:= 1, R mod 3 =:= 1, R mod 4 =:= 1.




macarons(N) :- between(2, 100, N),  N mod 2 =:= 1, N mod 3 =:= 1, N mod 4 =:= 1, N mod 5 =:= 1,  N mod 6 =:= 1.


dupli([], []).
dupli([X|R], [X,X|Y]):- dupli(R,Y).

syracuse(1, 1):- write('1').
syracuse(N, L):- N>1, N mod 2 =:= 0, write(N), write(' '), N1 is N//2, syracuse(N1, R), L is R+1.
syracuse(N, L):- N>1, N mod 2 =:= 1, write(N), write(' '), N1 is (N*3)+1, syracuse(N1, R), L is R+1.

melange(R, [], R).
melange([], R, R).
melange([X|R], [Y|L], [X,Y|R1]):- melange(R, L, R1).

pgcd(X, 0, X).
pgcd(X, Y, Z):- Y>0,Z1 is X mod Y, pgcd(Y, Z1, Z).


chiffre(X) :- between(0, 9, X).


queDesZeros([]).
queDesZeros([0|R]):- queDesZeros(R).

valeur([], 0).
valeur([X|R], N):- valeur(R, Z), length(R, R1), N is Z + X*2**(R1).

score([], [], 0).
score([X|R], [A|L], N):- X==A, score(R, L, N1), N is N1+1.
score([X|R], [A|L], N):- X\== A, score(R, L, N).

qcm([A, B, C, D, E, F, G, H, I, J]):- member(A, [a,b,c,d]),
    member(B, [a,b,c,d]),
    member(C, [a,b,c,d]),
    member(D, [a,b,c,d]),
    member(E, [a,b,c,d]),
    member(F, [a,b,c,d]),
    member(G, [a,b,c,d]),
    member(H, [a,b,c,d]),
    member(I, [a,b,c,d]),
    member(J, [a,b,c,d]),
    score([b,c,b,a,c,c,c,d,c,c],[A, B, C, D, E, F, G, H, I, J], 7),
    score([b,d,c,a,d,d,c,c,a,b],[A, B, C, D, E, F, G, H, I, J], 6),
    score([d,a,b,b,d,d,c,d,a,b],[A, B, C, D, E, F, G, H, I, J], 5),
    score([c,d,c,b,d,b,b,c,a,a],[A, B, C, D, E, F, G, H, I, J], 3).



swap([], []).
swap([R], [R]).
swap([X,Y|R], [Y,X|A]):- swap(R, A).

nieme(1, [X|_], X).
nieme(N, [_|X], R):- N>1, N1 is N-1, nieme(N1, X, R).

nb([], _, _, 0).
nb([X|R], Min, Max, N):- X >= Min, X < Max, nb(R, Min, Max, N1), N is N1+1.
nb([_|X], Min, Max, N):- nb(X, Min, Max, N).

motPrec(_, [], []).
motPrec(X, [Y, Z|R], [Y|L]):- X==Z, motPrec(X, [Z|R], L).
motPrec(X, [_ | R], L) :- motPrec(X, R, L).

tousDiff([_]).
tousDiff([X, Y|R]):- X\=Y, Y\=X, tousDiff([Y|R]).

union([], L, L).
union([X | Reste1], L2, Resultat) :-
    member(X, L2),
    !,
    union(Reste1, L2, Resultat).
union([X | Reste1], L2, [X | Resultat]) :-
    union(Reste1, L2, Resultat).


intersection([], _, []).
intersection([X | Reste1], L2, [X | Resultat]) :-
    member(X, L2),
    intersection(Reste1, L2, Resultat).
intersection([_ | Reste1], L2, Resultat) :-
    intersection(Reste1, L2, Resultat).


mylast(X, [X]).
mylast(X, [_|R]):- mylast(X, R).

myla(X, [X, _]).
myla(X, [_|R]):- myla(X, R).

mylength([], 0).
mylength([_|R], N):- mylength(R, N1), N is N1 +1.

myappend([], L, L).
myappend(L, [], L).
myappend([X|R1], L, [X|R2]):- myappend(R1, L, R2).

myreverse([], []).

myreverse([X|R], L):- myreverse(R, L1), append(L1, [X], L).

compresse([X, X|R], L):- compresse([X|R], L).
compresse([X, Y|R], [X|L]):- X\=Y, compresse([Y|R], L).
compresse([], []).
compresse([X], [X]).

pack([], []).
pack([X], [[X]]).
pack([X,X|R], [[X|L]|P]):- pack([X|R], [L|P]).
pack([X, Y|R], [[X]|P]):- X\=Y, pack([Y|R], P).


replace([_|R], 1, X, [X|R]).
replace([X|L], A, B,[X|R]):- A>1, N is A-1, replace(L, N, B, R).

replacer([X|R], Y, A, L):- replace(X, Y, A, P), replacer(R, Y, A, N), append([P], N, L).
replacer([],_,_, []).

modifier([X|N], 1, B, Y,[R|N]):- replace(X, B, Y, R).


