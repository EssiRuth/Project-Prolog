animal(alligator).
animal(cheval).
animal(vache).

animal(caribour).
animal(ours).

mutant(L):-animal(X),animal(Y),atom_chars(X,LX),atom_chars(Y,LY),
 append(LX1,LX2,LX), not(LX1=[]),not(LX2=[]),
 append(LX2,LY1,LY),append(LX,LY1,L1),atom_chars(L,L1),not(animal(L)).

symboleSuivantE(X, Y):-
    name(X, [A,B]),
    A\=122,
    E1 is B+1,
    name(Y, [A,E1]).

symboleSuivantE(X, Y):-
    name(X, L),
    reverse(L, [122, A]),
    symboleSuivantE(A, Y).


unSymbole(S):-
    between(97, 122, L),
    name(S, [L]).

unSymbole(S):-
    name(S, [A]),
    A>122,
    atom_concat(X, Y, S),
    between(97,122,X),
    between(97,122,Y),
    name(X, [97]),
    name(Y, [98]).

filtre([], _,_,[]).
filtre([_|X], A, N, L):- filtre(X, A, N, L).

filtre([X|R], A, 1, [X|L]):-
    X<A,
    filtre(R, A, 1, L).

filtre([X|R], A, 0, [X|L]):-
    X>A,
    filtre(R, A, 0, L).
