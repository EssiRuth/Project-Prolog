%cr�ation de la matrice du plateau
plateau(Ligne, Colonne, P):-
    findall(Lig, (between(1, Ligne,_), length(Lig, Colonne), maplist(=(0), Lig)), P).

%affiche le contenu de chaque case en fonction d'un chiffre
affiche_case_plateau(1):- write('| X ').
affiche_case_plateau(0):- write('| I ').
affiche_case_plateau(2):- write('| O ').
%affiche la ligne enhaut
affiche_ligne_haut(Colonne):-
    write('+'),
    forall(between(1, Colonne, _), write('---+')), nl.

% affiche un trait pour chaque valeur entre 1 et le nombre de la ligne
% et affiche "|" � chaque ligne
affiche_val_lignes(Ligne):-
    maplist(affiche_case_plateau, Ligne),
    write('|'), nl.

%affiche la colonne et la ligne
affiche(Colonne, Ligne):-
    affiche_val_lignes(Ligne),
    affiche_ligne_haut(Colonne).

%affiche le plateau du jeu
affiche_plateau(Plateau):-
    length(Plateau, NbLigne),
    NbLigne>0,
    Plateau= [Ligne1|_],
    length(Ligne1, NbCol),
    %affiche_colonne(NbCol),
    affiche_ligne_haut(NbCol),
    maplist(affiche(NbCol), Plateau).

%compte le nombre d'allumettes enlev�s sur une ligne donn�e
compte_dispo([], 0).
compte_dispo([0|Reste], Res) :-
    compte_dispo(Reste, R),
    Res is R + 1.
compte_dispo([X|Reste], Res) :-
    X \= 0,
    compte_dispo(Reste, Res).

%Calcule la somme des valeurs d'une liste
somme_list([], 0).
somme_list([X], X).
somme_list([X,Y|R], Res):-
    R1 is X+Y, somme_list(R, R2), Res is R1+R2.

%retourne la valeur total disponible sur le plateau
compte_dispo_total(Plateau, Res):-
    maplist(compte_dispo, Plateau, R), somme_list(R, Res).

%Ecris le joueur a gang� ou perdu
partie_fin(Plateau):-
    gagne(Plateau, J),
    (   J=:=1 -> write('Bravo! vous avez gagn�')
    ;   J=:=2 -> write('Pas de chance ! Vous avez perdu !')),!.

%retourne vrai si le joueur J a gagn�
gagne(Plateau, J):-
    (   elt_pos(X,4,4,Plateau), X=J).

%retourne l'�l�ment X � la ligne Lig et colonne Col dans la matrice
elt_pos(X, Lig, Col, Matrice) :-
    nth1(Lig, Matrice, Row),
    nth1(Col, Row, X).

%Choix de l'ordinateur lors de la partie
choix_ordi(Plateau, ChoixN_ordi):-
    (   choix1(Plateau,ChoixN_ordi,Strategie);
    choix2(Plateau,ChoixN_ordi,Strategie);
    choix3(Plateau,ChoixN_ordi,Strategie);
    choix4(Plateau,ChoixN_ordi,Strategie)),
    choixA_ordi(Strategie).

%choix de l'ordinateur si l'utilisateur ne veut pas commencer
choix1(Plateau,ChoixN_ordi,Strategie):-
     nth1(1, Plateau, E),
     nth1(1, E, 0) -> random_between(1,3, ChoixN_ordi), Strategie=1.

% choix de l'ordinateur lorsque le nombre restant sur le plateau n'est
% pas un nombre multiple de 4
choix2(Plateau,ChoixN_ordi,Strategie):-
      \+ parite(Plateau)->
        compte_dispo_total(Plateau, Res),
        between(1,3, ChoixN_ordi),
        R1 is Res-ChoixN_ordi,
        R1 mod 4 =:= 0, Strategie=2.

% choix de l'ordinateur lorsque le nombre sur le plateau est un multiple
% de 4
choix3(Plateau,ChoixN_ordi,Strategie):-
    parite(Plateau) ->
        random_between(1,3, ChoixN_ordi), Strategie=3.

% choic de l'ordinateur lorsqu'il ne reste que un,deux, ou trois
% allumettes sur le plateau
choix4(Plateau,ChoixN_ordi,Strategie):-
    compte_dispo_total(Plateau, Res), (Res=:=1 ->ChoixN_ordi=1;
                                          Res=:=2 -> ChoixN_ordi=2;
                                          Res=:=3 -> ChoixN_ordi=3), Strategie=4.

%explique chaque choix de l'ordinateur
explique(1):-write("Je l'ai choisi al�atoirement puisqu'on est au d�but."), nl.
explique(2):-write("J'essaie de laisser un nombre pair sur le plateau. Un indice : ce nombre est multiple de quelque chose mais je ne vous dis pas hahaha !"),nl.
explique(3):-write("Je le prends al�atoirement. J'ai pas trop d'option"),nl.
explique(4):-write("Je prends le reste pour remporter la partie"),nl.

%efface les anciens choix et ins�re juste le dernier qu'il a fait
:- dynamic choixD_ordi/1.
choixA_ordi(ChoixN_ordi):-
    retractall(choixD_ordi(_)),
    assert(choixD_ordi(ChoixN_ordi)).
choixD_ordi(ChoixN_ordi):-
    choixD_ordi(ChoixN_ordi).

%verifie si le nombre d'allumettes sur le plateau est un multiple de 4
parite(Plateau):-
    compte_dispo_total(Plateau, Res),
    Res mod 4 =:= 0.

% met � jour le plateau en fonction de la ligne, colonne et la valeur
% donn�e et retourne le nouveau tableau
met_a_jour(Plateau, Ligne, Col, NvVal, NvPlateau):-
    nth1(Ligne, Plateau, A, Reste),
    replace(A, Col, NvVal, NewL),
    nth1(Ligne, NvPlateau, NewL, Reste).

%remplace un �l�ment d'une liste par un autre en sp�cifiant la position
replace([_|R], 1, X, [X|R]).
replace([X|L], A, B,[X|R]):- A>1, N is A-1, replace(L, N, B, R).

% v�rifie si la colonne est valide
valid_colonne(Colonne) :-
    Colonne >= 1, Colonne =< 4.

%obtient la ligne Lig et la colonne Col du plateau
est_vide(Plateau, Lig, Col) :-
    nth1(Lig, Plateau, Ligne),
    nth1(Col, Ligne, Case),
    Case =:= 0.

%met a jour le plateau successivement avec les choix des joueurs
met_a_jour_plateau(Plateau, L, C, Choix, R, J) :-
    valid_colonne(C),
    est_vide(Plateau, L, C),
    !,
    (   Choix > 0
    ->  met_a_jour(Plateau, L, C, J, R1),
        Choix1 is Choix - 1,
        (   C1 is C + 1,
            met_a_jour_plateau(R1, L, C1, Choix1, R,J)
        ;   R = R1
        )
    ;   R = Plateau
    ),!.

met_a_jour_plateau(Plateau, L, C, Choix, R, J) :-
    valid_colonne(C),
    !,
    C1 is C + 1,
    met_a_jour_plateau(Plateau, L, C1, Choix, R, J),!.

met_a_jour_plateau(Plateau, L, _, Choix, R, J) :-
    L1 is L + 1, L=<4,
    met_a_jour_plateau(Plateau, L1, 1, Choix, R, J),!.

% demande � l'utilisateur ses choix et lui r�pond quand il demande le
% pourquoi du choix de l'ordinateur
choix_j(ChoixN):-
    (   repeat,
        write('Combien d''allumettes veux-tu enlever entre 1 et 3? (Tapez 1, 2 ou 3) : '), nl,
    read(Input),nl,
    (   Input=pourquoi -> choixD_ordi(Strategie),explique(Strategie),nl,fail;
   (    number(Input), input_prop(Input)-> ChoixN=Input))).

%v�rifie si ce que l'utilsateur a donn� est conforme
input_prop(Choix):- number(Choix), member(Choix, [1,2,3]).
input_prop(Choix):- not(member(Choix, [1,2,3])), write('Proposez un chiffre entre 1,2,et 3 s''il vous pla�t'), nl,fail.

%demande � l'utlisateur s'il veut rejouer la partie
rejouer(Plateau):-
    write('Voulez-vous rejouer? (Tapez oui ou non) : '),
    read(Input),
    (   Input=non -> !;
        Input=oui -> principale);
    (   write('Entr�e non valide!'), nl, rejouer(Plateau)).

%jouer le choix du joueur J
jouer(Plateau, ChoixJ, NvPlateau, J):-
    met_a_jour_plateau(Plateau,1, 1, ChoixJ, NvPlateau, J).

%tour de jeu pour le joueur 1, l'utilisateur
tour_joueurs(Plateau, NvPlateau, joueur):-
    choix_j(ChoixN),
    jouer(Plateau, ChoixN, NvPlateau, 1).

%tour de jeu pour le joueur 2, l'ordi
tour_joueurs(Plateau, NvPlateau, ordi):-
    choix_ordi(Plateau, ChoixN_ordi),
    jouer(Plateau, ChoixN_ordi, NvPlateau, 2),
    write('Je choisis d''enlever'), write(' '), write(ChoixN_ordi), write(' '), write('allumettes'),nl, nl.

% jeu tour � tour, ici le joueur qui est l'utilisateur joue son tour,
% ensuite c'est le tour de l'ordi
play(Plateau, joueur):-
    partie_fin(Plateau) -> nl,
    rejouer(Plateau);
    (   tour_joueurs(Plateau, NvPlateau, joueur),
        affiche_plateau(NvPlateau),nl,
     play(NvPlateau, ordi)).

% jeu tour � tour, ici l'ordinateur joue son tour et apr�s l'utilisateur
% qui est le joueur joue
play(Plateau, ordi):-
    partie_fin(Plateau) -> nl,
    rejouer(Plateau);
    (   tour_joueurs(Plateau, NvPlateau, ordi),
        affiche_plateau(NvPlateau),nl,
     play(NvPlateau, joueur)).

%jeu tour � tour en fonction du joueur qui joue
play(Plateau, _):-
     (  afficher_game,
       tour_joueurs(Plateau, NvPlateau, joueur),
     afficher_game,
     play(NvPlateau, ordi)).

%v�rifie si ce que l'utilisateur a donn� est conforme
analyse(R):- member(R, [oui, non]).
analyse(R):- not(member(R, [oui, non])), write('Veuillez �crire oui ou non s''il vous pla�t!'), nl, fail.

%affiche le plateau du jeu
afficher_game:-
    plateau(4,4,Plateau),
    affiche_plateau(Plateau).

%demande � l'utilisateur s'il veut commencer la partie ou pas
demande_partie(Plateau):-
    write('Voulez-vous commencez la partie? tapez(oui ou non):'),nl,
    read(Input), analyse(Input),
    (   Input=non -> play(Plateau, ordi);
        Input=oui -> play(Plateau, joueur));


     demande_partie(Plateau).

% pr�dicat pour jouer le jeu
principale:-
    plateau(4, 4, P),
    afficher_game, nl,
    demande_partie(P).






