ages(X, Y):- between(10, 99, X), between(10, 99, Y), X<Y, U is X mod 10, D is (X//10) mod 10, X is U + D*10, Y is D + U*10, Y-1 =:= 2 * (X-1), X\=Y, Y\=X.


a(0,Y):-!,c(Y).
a(X,Y):-b(X,Y),!,c(X).
a(_,_).
b(1,2).
b(1,3).
b(2,5).
c(1).
c(2).
c(3).



diff([], [], []).
diff([X|R], [Y|L], [Z|M]) :- Z is X-Y, diff(R, L, M).


repete(_, 0, []).
repete(X, Y, [X|R]):- Y>0, Y1 is Y-1, repete(X, Y1, R).

eclate([], _, []).
eclate([], 0, []).
eclate([X|R], N, [Y|L]):- repete(X, N, Y), eclate(R, N, L).

superAppend([], []).
superAppend([X|R], L):- append(X, R1, L), superAppend(R, R1) .


somme([X|R], N):- somme(R, N1), N is N1+X.
 partition(L,[X|R],[Y|T]):-
 member(X,L),member(R,L),member(Y,L),member(T,L),X=<N,R=<N,Y=<N,T=<N.
 partition(L,[X|R],[Y|T]):-
 member(X,L),member(R,L),member(Y,L),member(T,L),X>N,R>N,Y>N,T>N.
aplatir([], []).


croissant([_]).
croissant([X,Y|Z]):- X<Y, croissant([Y|Z]).


filtre([],_,_,[]).
filtre([X|A], N, 1, [X|Y]):- X<N,!, filtre(A, N, 1, Y).
filtre([X|A], N, 0, [X|Y]):- X>N,!, filtre(A, N, 0, Y).
filtre([_|A], N, V, Y):-  filtre(A, N, V, Y),!.

%appartientListe([],_).
% appartientListe([X|R], [X|L]):- append(_, Reste,L), append([X|R],_,
% Reste).
%appartientListe(X, [_|L]):-  appartientListe(X, L).


%verifHorizontale([],_).

verifHorizontale([], _).
%verifHorizontale([MotHead|ResteMot], [Ligne|ResteGrille]) :-
%    append(_, [MotHead|_], Ligne),
%    append([MotHead|ResteMot], _, Ligne);
%    verifHorizontale([MotHead|ResteMot], ResteGrille).

verifHorizontale(X, [Ligne|_]) :-
    appartientListe(X, Ligne).
    verifHorizontale(X, [_|R]):- verifHorizontale(X, R).

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


insere(X, [], [X]).
insere(X, [Y|Ys], [X,Y|Ys]) :-
    X =< Y.
insere(X, [Y|Ys], [Y|Zs]) :-
    X > Y,
    insere(X, Ys, Zs).

p(1).
p(X):-q(Y,Y),X is Y/2.
p(3).
q(2,4).
q(3,4).
q(4,4).
q(6,6).
%p(Y), Y=1, Y=2, coupure
%p(Y), Y=1, Y=2, Y=3, Y=3

nb([], _, 0). % Cas de base : la première liste est vide, le nombre d'éléments est 0.
nb([X|Xs], Liste, R) :-
    member(X, Liste), % Vérifie si X appartient à Liste
    nb(Xs, Liste, R1), % Récursivement, cherche le nombre d'éléments dans la queue de la première liste
    R is R1 + 1,!. % Incrémente le résultat si X est trouvé dans Liste
nb([_|Xs], Liste, R) :-
    nb(Xs, Liste, R),!.


isoleElements([], []).
isoleElements([X|Xs], [[X]|ListeResultat]) :-
    isoleElements(Xs, ListeResultat).
%isoleElements([4,1,7,3,9,11,6],R).
%R = [[4], [1], [7], [3], [9], [11], [6]].


fusionne([], L, L).
fusionne(L, [], L).

fusionne([[K,V]|T1], L2, [[K,V]|R]) :-
    fusionne(T1, L2, R),!.

fusionne([[K,V]|T1], L2, R) :-
    member([K,V2], L2),
    NV is V + V2,
    delete(L2, [K,V2], NL2),
    fusionne(T1, NL2, R1),
    R = [[K,NV]|R1],!.




diagonale(Matrice, Somme) :-
    diagonale(Matrice, 1, Somme).

diagonale([], _, 0).
diagonale([Ligne|Reste], Index, Somme) :-
    nth1(Index, Ligne, Elem),
    NextIndex is Index + 1,
    diagonale(Reste, NextIndex, ResteSomme),
    Somme is ResteSomme + Elem.



symetrique(Matrice) :-
    transpose(Matrice, Transposee),
    Matrice = Transposee.


listS([], 0).
listS([X], X).
listS([X,Y|R], Res):-
    R1 is X+Y, listS(R, R2), Res is R1+R2.

listP([], 0).
listP([X|R], A):-
 listS(X, A1),
 listP(R, R1),
 A is A1+R1.



%Recherche si un mot est caché dans une grille, horizontalement       */
/* ou verticalement. Ne recherche que de gauche à droite ou haut en bas */
/* Exemple : grille(T),motCache([a,n,e],T).                                         */

grille([
[a,v,a,y],
[b,c,n,o],
[b,n,e,c],
[r,v,c,i]]).

/* Predicat principal. Recherche horizontalement sur la grille puis    */
/* une nouvelle fois sur la grille transposée, ce qui revient à        */
/* chercher verticalement                                              */
motCache(L,Grille):-verifHor(L,Grille),!.
motCache(L,Grille):-inverseLigCol(Grille,GrilleInv),verifHor(L,GrilleInv).

nbOcc(X, N):- grille(G), findall(X, motCache(X, G), Occ), length(Occ, N).
/* recherche horizontale                                               */
verifHor(M,[L1|_]):-appartientListe(M,L1),!.
verifHor(M,[_|L2]):-verifHor(M,L2).


/* vrai si les elements de la premiere liste se retrouvent dans le     */
/*  meme ordre quelque part dans la seconde :                          */
/* soit c'est un prefixe, soit c'est plus loin                         */
appartientListe(L1,L2):-prefixe(L1,L2),!.
appartientListe(L1,[_|L2]):-appartientListe(L1,L2).

/* prefixe verifie si les elements de la premiere liste se retrouvent  */
/* au debut (et seulement au debut) de la seconde                      */
prefixe([],_).
prefixe([X|L1],[X|L2]):-prefixe(L1,L2).

/* Transposition de la matrice */

inverseLigCol([[]|_],[]).
inverseLigCol(L,[Premiers|R]):-separePremiersRestes(L,Premiers,Restes),
inverseLigCol(Restes,R).

separePremiersRestes([],[],[]).
separePremiersRestes([[X|L1]|LR],[X|P],[L1|R]):-separePremiersRestes(LR,P,R).


/* AUTRE SOLUTION                                                       */
/* inversion des lignes et des colonnes                                */
/* inversion = inversion à partir de la colonne 1                      */
inverseLigCol2(L,R):-inverseLigCol2(L,R,1).

/* inversion à partir de la colonne N                                   */
/* cas de base : on est à la dernière colonne                           */
/* cas général : on cherche tous les éléments de toutes les sous-listes */
/* qui sont à la position N. On relance ensuite avec N+1                */
inverseLigCol2(L,[],N):-length(L,Long),N is Long+1,!.
inverseLigCol2(L,[L1|R],N):-findall(X,(member(SL,L),nth1(N,SL,X)),L1),
     N1 is N+1,inverseLigCol2(L,R,N1).

consonnes([b,c,d,f,g,h,j,k,l,m,n,p,q,r,s,t,v,w,x,y,z]).

voyelles([a,e,i,o,u]).

nouveauMot(N, Mot) :-
    nouveauMot(N, 0, [], Mot).

nouveauMot(N, N, Acc, Mot) :-
    reverse(Acc, Mot).

nouveauMot(N, Index, Acc, Mot) :-
    Index < N,
    IndexMod2 is Index mod 2,
    (IndexMod2 =:= 0 -> % si l'index est pair
        consonnes(Consonnes),
        length(Consonnes, LengthConsonnes),
        random(0, LengthConsonnes, RandomIndex),
        nth0(RandomIndex, Consonnes, Char)
    ; % sinon
        voyelles(Voyelles),
        length(Voyelles, LengthVoyelles),
        random(0, LengthVoyelles, RandomIndex),
        nth0(RandomIndex, Voyelles, Char)
    ),
    NewIndex is Index + 1,
    nouveauMot(N, NewIndex, [Char|Acc], Mot).


profondeur(X, 0) :-
    \+ is_list(X).
profondeur([], 1).
profondeur([T|Q], R) :-
    is_list(T),
    !,
    profondeur(T, RT),
    profondeur(Q, RQ),
    R is max(RT, RQ).
profondeur([_|Q], R) :-
    profondeur(Q, RQ),
    R is RQ.
