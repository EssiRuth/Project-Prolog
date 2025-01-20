% Plateau de jeu exemple
plateau([
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o
]]).

% Prédicat pour afficher un plateau de jeu dans un format visuellement attrayant
afficher_plateau(P) :-
    nl,
    % Saut de ligne
    afficher_ligne_haut, % Affiche la ligne supérieure du plateau
    afficher_contenu(P),
    % Affiche le contenu des cases du plateau
     !. % Affiche la ligne inférieure du plateau

% Prédicat pour afficher une ligne horizontale
afficher_ligne_haut :-
    write('+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+'), nl.


% Prédicat pour afficher le contenu des cases du plateau
afficher_contenu([]).
afficher_contenu([Ligne|Reste]) :-
    afficher_ligne(Ligne),
    afficher_ligne_haut,
    afficher_contenu(Reste).


% Prédicat pour afficher une ligne de contenu
afficher_ligne([]) :- write('|'),nl.
afficher_ligne([Case|Reste]) :-
    write('|'),afficher_contenu_case(Case),
    afficher_ligne(Reste).

% Prédicat pour afficher le contenu d'une case
afficher_contenu_case(vide) :-
    write('').
afficher_contenu_case(Contenu) :-
    format(' ~w', [Contenu]).


demander_props(ChoixCol):-
    repeat,
    write('Combien de rond veux-tu enlever entre 1 et 3? (Tapez 1, 2 ou 3) : '),
    read_line_to_string(user_input, Input),
    number_string(Choix, Input), input_props(Choix),!, ChoixCol=Choix.

%vérifie si ce que l'utilsateur a donné est conforme
input_props(Choix):- number(Choix), member(Choix, [1,2,3]).
input_props(Choix):- not(member(Choix, [1,2,3])), write('Proposez un chiffre entre 1,2,et 3 s''il vous plaît'), nl,fail.
% input_props(Choix):-(not(number(Choix))), write('Ce n''est pas un
% chiffre'), nl, fail.


%remplace un élément d'une position par autre chose spécifiée
replace([_|R], 1, X, [X|R]).
replace([X|L], A, B,[X|R]):- A>1, N is A-1, replace(L, N, B, R).



remplacer(P,3,[L]):-nth1(1,P, R1), between(1,17, A), between(1, 17, B), between(1, 17, C), not(est_enleve(P,A)), not(est_enleve(P,B)), not(est_enleve(P,C)),  replace(R1, A, x, L1), replace(L1, B, x, L2), replace(L2,C,x, L).

remplacer(P,2,[L]):-nth1(1,P, R1), between(1,17, A), between(1, 17, B), not(est_enleve(P,A)), not(est_enleve(P,B)), replace(R1, A, x, L1), replace(L1, B, x, L),!.

remplacer(P,1,[L]):-nth1(1,P, R1), between(1,17, A), not(est_enleve(P,A)),  replace(R1, A, x, L),!.

mettre_a_jour(Plateau,X, NvPlateau):-
    (   X=1 -> remplacer(Plateau, 1, NvPlateau)
    ;   X=2 -> remplacer(Plateau, 2, NvPlateau)
    ;   X=3 -> remplacer(Plateau, 3, NvPlateau)),!.

est_enleve(P,R):- nth1(1, P, L), nth1(R, L, x);
% Prédicat pour compter le nombre d'occurrences de O dans une liste
nb_occurence([], 0).
nb_occurence([o|Reste], Res) :-
    nb_occurence(Reste, R),
    Res is R + 1.
nb_occurence([X|Reste], Res) :-
    X \= o,
    nb_occurence(Reste, Res).

verifie_nb(P,Pos):-
    nth1(1, P, L),
    nb_occurence(L, Res),
    Res>Pos.
%retourne vrai si la partie est gagné et faux sinon

victoire(Plateau,Choix_j, 1):-
    mettre_a_jour(Plateau, Choix_j, NvPlateau),
    nth1(1, NvPlateau, L),
    nb_occurence(L,0).
victoire(Plateau, Choix_j, 2):-
    mettre_a_jour(Plateau, Choix_j, NvPlateau),
    nth1(1, NvPlateau, L),
    nb_occurence(L,0).




%jouer le coup pour la ou les positions données par le joueur
%Enlever les positions qu'il a données
%Expliquer stratégie au joueur
%Choix du coup par l'IA
%dynamic dernier coup IA
%jouer tour IA, le tour de l'IA, après le joueur
%jouer jeu ia
%jouer jeu joueur
%jouer jeu, dit qui a gagné
%verifie si joueur 1 a gagné, gagne(Plateau, J1)
%affiche_jeu
%enlève sur le plateau la ou les positions spécifiées


%demande au joueur s'il veut
