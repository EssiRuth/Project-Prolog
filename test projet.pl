%création de la matrice du plateau
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

%affiche un trait pour chaque valeur de la ligne
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

%retourne la Numième ligne du plateau
ligne(P, Num, Ligne):-
    nth1(Num, P, Ligne).


%remplace un élément d'une liste par un autre en spécifiant la position
replace([_|R], 1, X, [X|R]).
replace([X|L], A, B,[X|R]):- A>1, N is A-1, replace(L, N, B, R).



%demande à l'utilisateur s'il veut commencer la partie ou pas
%demande(Plateau):-
%    write('Voulez-vous commencez la partie? oui ou non? :'),
%    read(Input),
%    (   Input=non -> jouer_jeu(
%   atom_codes(AtomChoix, Input), analyse(AtomChoix),!,
%   Reponse=AtomChoix)).

% vérifie si une ligne du plateau est déjà enlevé
ligne_enleve(L):-
    compte_dispo(L, Res), Res=0.


%compte le nombre de rond enlevé sur une ligne donnée
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

%Ecris la personne qui a gagné
partie_fin(Plateau):-
    (   victoire(Plateau, 1) -> write('Bravo! Vous avez gagné')
    ;   victoire(Plateau, 2) -> write('Pas de chance! Vous avez perdu')),!.

%retourne vrai si le joueur J a gagné
victoire(Plateau, J):-
    compte_dispo_total(Plateau, 0),
    ligne(Plateau, 4, L),
    nth1(4, L, J).

%quand la réponse du joueur est oui
choix1_ordi(Plateau, Reponse, Choix_j, ChoixN_ordi, Choix1):-
    Reponse=oui,
    compte_dispo_total(Plateau, Res),
    Res>0,
    ChoixN_ordi is 4-Choix_j, Choix1=1.

%quand la réponse est non
choix2_ordi(Plateau, Reponse, _, ChoixN_ordi, Choix2):-
    Reponse=non,
    random_between(1,3, Choix),
    (   parite(Plateau) -> ChoixN_ordi = Choix
    ;   compte_dispo_total(Plateau, Res),
        \+parite(Plateau),
        between(1,3,ChoixN_ordi),
        R1 is Res-ChoixN_ordi,
        R1 mod 4 =:=0), Choix2=2.

% les choix que l'ordi doit faire en fonction de la réponse et du choix
% du joueur
choix_coup_ordi(Plateau,Reponse,ChoixN_ordi):-
    (   choix1_ordi(Plateau, Reponse, _, ChoixN_ordi, _)
   ;     choix2_ordi(Plateau, Reponse, _, ChoixN_ordi, _)).
    %choixA_ordi(Choix).

:- dynamic choixD_ordi/1.
choixA_ordi(ChoixN):-
    retractall(choixD_ordi(_)),
    assert(choixD_ordi(ChoixN)).
choixD_ordi(ChoixN):-
    choixD_ordi(ChoixN).

%verifie si le nombre sur le plateau est un multiple de 4
parite(Plateau):-
    compte_dispo_total(Plateau, Res),
    Res mod 4 =:= 0.


% met à jour le plateau en fonction de la ligne, colonne et la valeur
% donnée et retourne le nouveau tableau
met_a_jour(Plateau, Ligne, Col, NvVal, NvPlateau):-
    nth1(Ligne, Plateau, A, Reste),
    replace(A, Col, NvVal, NewL),
    nth1(Ligne, NvPlateau, NewL, Reste).


% vérifie si la colonne est valide
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
    L1 is L + 1,
    met_a_jour_plateau(Plateau, L1, 1, Choix, R, J),!.

%demande à l'utilisateur ses choix
choix_j(ChoixN):-
    write('Combien de rond veux-tu enlever entre 1 et 3? (Tapez 1, 2 ou 3) : '),
    read(Input),
    (  number(Input), input_prop(Input) -> ChoixN=Input)
    ;(   writeln('Entrée non valide'),
     choix_j(ChoixN)).



%vérifie si ce que l'utilsateur a donné est conforme
input_prop(Choix):- number(Choix), member(Choix, [1,2,3]).
input_prop(Choix):- not(member(Choix, [1,2,3])), write('Proposez un chiffre entre 1,2,et 3 s''il vous plaît'), nl,fail.

rejouer(Plateau):-
    write('Voulez-vous rejouer? (Tapez oui ou non) : '),
    read(Input),
    (   Input=non -> !;
        Input=oui -> jeu);
    (   write('Entrée non valide!'), nl, rejouer(Plateau)).

%jouer le choix du joueur J
jouer(Plateau, ChoixJ, NvPlateau, J):-
    met_a_jour_plateau(Plateau,1, 1, ChoixJ, NvPlateau, J).

%tour de jeu pour le joueur 1, l'utilisateur
jeu_tour(Plateau,_, NvPlateau, joueur):-
    choix_j(ChoixN),
    jouer(Plateau, ChoixN, NvPlateau, 1).

%tour de jeu pour le joueur 2, l'ordi
jeu_tour(Plateau, oui, NvPlateau, ordi):-

    choix_coup_ordi(Plateau, oui, ChoixN_ordi),
    jouer(Plateau, ChoixN_ordi, NvPlateau, 2).

jeu_tour(Plateau, non, NvPlateau, ordi):-

    choix_coup_ordi(Plateau, non, ChoixN_ordi),
    jouer(Plateau, ChoixN_ordi, NvPlateau).


%vérifie si ce que l'utilisateur a donné est conforme
analyse(R):- member(R, [oui, non]).
analyse(R):- not(member(R, [oui, non])), write('Veuillez écrire oui ou non s''il vous plaît!'), nl, fail.


afficher_jeu:-
    plateau(4,4,Plateau),
    affiche_plateau(Plateau).

jeu:-
    plateau(4, 4, Plateau),
    afficher_jeu,
    nl,
    write('Voulez-vous commencez la partie? tapez(oui ou non):'),
    read(Input),
    %(   Input=non -> joueur_jeu(Plateau,Input,ordi);
    %    Input=oui -> joueur_jeu(Plateau, _, joueur));
     analyse(Input),
     joueur_jeu(Plateau, Input,_).













% Prédicat pour organiser le tour de jeu du joueur humain
%joueur_jeu(Plateau, Reponse, Joueur) :-
%    partie_fin(Plateau), % Vérifier si la partie est terminée
%    rejouer(Plateau). % Afficher un message de fin et proposer de
%    rejouer

joueur_jeu(Plateau, Reponse, Joueur) :-
    Reponse = oui, % Si le joueur veut continuer
    Joueur = joueur, % Si c'est le tour du joueur humain
    jeu_tour(Plateau, Reponse, NvPlateau, Joueur), % Effectuer le tour de jeu
    affiche_plateau(NvPlateau), % Afficher le plateau mis à jour
    joueur_jeu(NvPlateau, oui, ordi). % Passer au tour de l'ordinateur

joueur_jeu(Plateau, Reponse, Joueur) :-
    Reponse = non, % Si le joueur veut arrêter
    Joueur = joueur, % Si c'est le tour du joueur humain
    jeu_tour(Plateau, Reponse, NvPlateau, Joueur), % Effectuer le tour de jeu
    affiche_plateau(NvPlateau), % Afficher le plateau mis à jour
    joueur_jeu(NvPlateau, non, ordi). % Passer au tour de l'ordinateur

% Prédicat pour organiser le tour de jeu de l'ordinateur
joueur_jeu(Plateau, _, _) :-
    partie_fin(Plateau), % Vérifier si la partie est terminée
    rejouer(Plateau).    % Afficher un message de fin et proposer de rejouer

joueur_jeu(Plateau, Reponse, Joueur) :-
    Reponse = oui, % Si le joueur veut continuer
    Joueur = ordi, % Si c'est le tour de l'ordinateur
    jeu_tour(Plateau, Reponse, NvPlateau, Joueur), % Effectuer le tour de jeu
    affiche_plateau(NvPlateau), % Afficher le plateau mis à jour
    joueur_jeu(NvPlateau, oui, joueur). % Passer au tour du joueur humain

joueur_jeu(Plateau, Reponse, Joueur) :-
    Reponse = non, % Si le joueur veut arrêter
    Joueur = ordi, % Si c'est le tour de l'ordinateur
    jeu_tour(Plateau, Reponse, NvPlateau, Joueur), % Effectuer le tour de jeu
    affiche_plateau(NvPlateau), % Afficher le plateau mis à jour
    joueur_jeu(NvPlateau, non, joueur). % Passer au tour du joueur humain
