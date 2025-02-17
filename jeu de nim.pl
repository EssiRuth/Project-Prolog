plateau(Ligne, Colonne, P):-
    findall(Lig, (between(1, Ligne,_), length(Lig, Colonne), maplist(=(0), Lig)), P).

%affiche le contenu de chaque case en fonction d'un chiffre

affiche_case_plateau(1):- write('| X ').
affiche_case_plateau(0):- write('| O ').

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


demander_props(ChoixCol):-
    repeat,
    write('Combien de rond veux-tu enlever entre 1 et 3? (Tapez 1, 2 ou 3) : '),
    read_line_to_string(user_input, Input),
    number_string(Choix, Input), input_props(Choix),!, ChoixCol=Choix.

%v�rifie si ce que l'utilsateur a donn� est conforme
input_props(Choix):- number(Choix), member(Choix, [1,2,3]).
input_props(Choix):- not(member(Choix, [1,2,3])), write('Proposez un chiffre entre 1,2,et 3 s''il vous pla�t'), nl,fail.
% input_props(Choix):-(not(number(Choix))), write('Ce n''est pas un
% chiffre'), nl, fail.


%remplace un �l�ment d'une position par autre chose sp�cifi�e
replace([_|R], 1, X, [X|R]).
replace([X|L], A, B,[X|R]):- A>1, N is A-1, replace(L, N, B, R).

% remplace les positions dans le plateau par des x en sp�cifiant la
% position
replacer(R,3, A, B, C, [L]):-nth1(1, R, R1), replace(R1, A, 1, L1), replace(L1, B, 1, L2), replace(L2,C,1, L).
replacer(R,2, A, B, [L]):-nth1(1, R, R1), replace(R1, A, 1, L1), replace(L1, B, 1, L).
replacer(R,1, A, [L]):- nth1(1, R, R1),replace(R1,A , 1, L).


%verifie si les positions ne sont pas encore enlev�s
verifie(P,N):- est_enleve(P,N), write('Cette position a d�j� �t� enlev�, Veuillez saisir une autre!'),nl, fail.
verifie(P,N):- not(est_enleve(P,N)), N\=0.
verifie(_,N):- N=0, write('Choissez un nombre entre 1 et 16 s''il vous pla�t'), nl, fail.


verifie(P,N1, N2):-verifie(P,N1), verifie(P,N2), N1\=N2.
verifie(P,N1, N2):- verifie(P,N1), verifie(P,N2), N1=N2, write('Vous avez d�j� propos� �a'), nl, fail.


% lit la position donn�� par l'utilisateur et v�rifie si cette position
% n'a pas encore �t� enlev�e


lire_nombre(P,Nombre) :-
    repeat,
    write('Entrez la position : '),
    read_line_to_string(user_input, Input),
    number_string(Nbre, Input),
    verifie(P,Nbre),
    Nombre=Nbre,!.

lire_nombre(P,Nbre1, Nbre2):-
    repeat,
    write('Entrez la premi�re position : '),
    read_line_to_string(user_input, Input),
    number_string(Nb1, Input),
    verifie(P,Nb1),
    write('Entrez la seconde position : '),
    read_line_to_string(user_input, Inpu),
    number_string(Nb2, Inpu),
    verifie(P,Nb2),
    verifie(P,Nb1,Nb2),
    Nbre1 =Nb1,
    Nbre2=Nb2,!.


lire_nombre(P,Nbre1, Nbre2, Nbre3):-
    repeat,
    write('Entrez la premi�re position : '),
    read_line_to_string(user_input, Input),
    number_string(Nb1, Input),
    verifie(P,Nb1),
    write('Entrez la seconde position : '),
    read_line_to_string(user_input, Inpu),
    number_string(Nb2, Inpu),
    verifie(P,Nb2),
    verifie(P,Nb1, Nb2),
    write('Entrez la troisi�me position : '),
    read_line_to_string(user_input, Inp),
    number_string(Nb3, Inp),
    verifie(P,Nb3),
    verifie(P,Nb1, Nb3),
    verifie(P,Nb2,Nb3),
    Nbre1=Nb1,
    Nbre2=Nb2,
    Nbre3=Nb3,!.

% Pr�dicat pour compter le nombre d'occurrences de O dans une liste
nb_occurence([], 0).
nb_occurence([0|Reste], Res) :-
    nb_occurence(Reste, R),
    Res is R + 1.
nb_occurence([X|Reste], Res) :-
    X \= 0,
    nb_occurence(Reste, Res).
% Pr�dicat pour obtenir les positions des z�ros dans une liste

positions_des_zeros(Liste, Positions) :-
    positions_des_zeros(Liste, 1, Positions),!.

% Cas de base : liste vide
positions_des_zeros([], _, []).

% Si la t�te de la liste est z�ro, ajouter la position � la liste des positions
positions_des_zeros([0|Reste], Indice, [Indice|PositionsRestantes]) :-
    NouvelIndice is Indice + 1,
    positions_des_zeros(Reste, NouvelIndice, PositionsRestantes).

% Si la t�te de la liste n'est pas z�ro, ignorer et passer � l'�l�ment suivant
positions_des_zeros([_|Reste], Indice, Positions) :-
    NouvelIndice is Indice + 1,
    positions_des_zeros(Reste, NouvelIndice, Positions).


positions_valides(Plateau, R):-
    nth1(1, Plateau, P),
    positions_des_zeros(P, R).

verifie_nb(P,Pos):-
    nth1(1, P, L),
    nb_occurence(L, Res),
    Res>Pos.
%retourne vrai si la partie est gagn� et faux sinon
gagne(P):-
    plateau(P),
    nth1(1, P, L),
    nb_occurence(L, Res),
    Res=0.
%jouer le coup pour la ou les positions donn�es par le joueur
%Enlever les positions qu'il a donn�es
%Expliquer strat�gie au joueur
%Choix du coup par l'IA
%dynamic dernier coup IA
%jouer tour IA, le tour de l'IA, apr�s le joueur
%jouer jeu ia
%jouer jeu joueur
%jouer jeu, dit qui a gagn�
%verifie si joueur 1 a gagn�, gagne(Plateau, J1)
%affiche_jeu
%enl�ve sur le plateau la ou les positions sp�cifi�es
enleve(Plateau,1, A, NouvPlateau):- nth1(1,Plateau, L),nb_occurence(L, Res), Res>1, replacer(Plateau,1,A, NouvPlateau).
enleve(Plateau,2, A,B, NouvPlateau):- nth1(1,Plateau, L),nb_occurence(L, Res), Res>2, replacer(Plateau,2,A,B, NouvPlateau).
enleve(Plateau,3, A,B,C,NouvPlateau):- nth1(1,Plateau, L),nb_occurence(L, Res), Res>3, replacer(Plateau,3,A,B, C, NouvPlateau).

%retourne vrai si une position R dans le plateau a d�j� �t� enlev�
est_enleve(P,R):- nth1(1, P, L), nth1(R, L, x).

%retire les ronds des positions donn�es et affiche le nouveau plateau
retirer1(P, P):-
    lire_nombre(R,Nb),
    enleve(R,1, Nb, P).
    %afficher_plateau(P),!.
retirer2(R, P):-
    lire_nombre(R,Nb1, Nb2),
    enleve(R,2, Nb1, Nb2, P).
   % afficher_plateau(P),!.
retirer3(R, P):-
    lire_nombre(R,Nb1, Nb2, Nb3),
    enleve(R,3, Nb1, Nb2, Nb3, P).
   % afficher_plateau(P),
    %!
met_a_jour(R, X, P):-
     (   ((X=1) -> enleve(R,1)
    ;(X=2) -> retirer2(R,P)
    ;(X=3) -> retirer3(R,P)).

%retire pour les positions donn�es du joueur
retire(R, X,P):-

    %demander_props(X),
    (   ((X=1) -> retirer1(R,P))
    ;(X=2) -> retirer2(R,P)
    ;(X=3) -> retirer3(R,P)).

%met � jour le plateau initial et affiche le nouveau tableau
met_a_jour(P,X,NouvPlateau):-
    retire(P,X,NouvPlateau),
    afficher_plateau(NouvPlateau),!.

%demande au joueur s'il veut commencer
demande(Reponse):-
    repeat,
    write('Voulez-vous commencez la partie? oui ou non? :'),
    read_line_to_string(user_input, Input),
    atom_codes(AtomChoix, Input), analyse(AtomChoix),!, Reponse=AtomChoix.%input_prop(AtomChoix),!, ChoixRond= At

analyse(R):- member(R, [oui, non]).
analyse(R):- not(member(R, [oui, non])), write('Veuillez �crire oui ou non s''il vous pla�t!'), nl, fail.

jouerr(Plateau, Choix_j, Reponse, Choix_ordi):-
    (   Reponse = oui -> demander_props(Choix_j),
        retire(Plateau, Choix_j, NvPlateau),
        afficher_plateau(NvPlateau),
        Choix_ordi is 4-Choix_j).
choix_joueur(Plateau, Choix, Pos):-
    demander_props(Choix),
    (   Choix = 1, lire_nombre(Plateau, Nb1),Pos=[Nb1]
    ;   Choix = 2, lire_nombre(Plateau, Nb1, Nb2), Pos=[Nb1, Nb2]
    ;   Choix = 3, lire_nombre(Plateau, Nb1, Nb2, Nb3), Pos=[Nb1, Nb2, Nb3]).
choix_ordi(Plateau, Choix_joueur, Reponse, choix_ordi):-
    (   Reponse = oui ->

    %write('J''enl�ve'), write(' '), write(X).
%jouer
%choix du coup par l'ia
%jouer un coup donn�e par le joueur
%jouer_tour(Plateau, joueur, NouveauPlateau):-
choix_position_valide(Plateau, [X|R]):-
    \+ est_enleve(Plateau, X), choix_position_valide(Plateau, R).

% jouer un coup dans la position donn�e par le joueur J=1, l'utilisateur
% et J=2, l'ia.
jouer(Plateau,J, X, NouvPlateau):-
    (   J=1 -> demander_props(X),
        retire(Plateau, X, NouvPlateau)
    ;   J=2 -> strategie1(X),
        retire(Plateau, X, NouvPlateau)).

%jouer(Plateau,joueur,X, NouvPlateau):-
%    demander_props(X),
%   retire(Plateau, X, NouvPlateau).



%jouer tour � tour
jouer_tour(Plateau, ia, NouvPlateau):-

    jouer(Plateau, 1,_, NouvPlateau).

jouer_tour(Plateau, joueur, NouvPlateau):-
    strategie1(X),
    jouer(Plateau, 1, X, NouvPlateau).

%jouer le jeu
play(Plateau, ia):-
    jouer_tour(Plateau, ia, NouvPlateau),
    afficher_plateau(NouvPlateau),
    play(NouvPlateau, joueur).


play(Plateau, joueur):-
    jouer_tour(Plateau, joueur, NouvPlateau),
    play(NouvPlateau, ia).

play(Plateau,_):-
    (   affiche_jeu_plateau,
        jouer_tour(Plateau, joueur, NouvPlateau),
        affiche_jeu_plateau,
        play(NouvPlateau,ia)).


affiche_jeu_plateau:-
    plateau(P),
    afficher_plateau(P).
