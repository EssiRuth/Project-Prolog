figures_pendu([
    '''
    +---+
    |   |
        |
        |
        |
        |
============
    ''',
    '''
    +---+
    |   |
    O   |
        |
        |
        |
============
    ''',
    '''
    +---+
    |   |
    O   |
   /    |
        |
        |
============
    ''',
    '''
    +---+
    |   |
    O   |
   / \\  |
        |
        |
============
    ''',
    '''
    +---+
    |   |
    O   |
   /|\\  |
        |
        |
============
    ''',
    '''
    +---+
    |   |
    O   |
   /|\\  |
   /    |
        |
============
    ''',
    '''
    +---+
    |   |
    O   |
   /|\\  |
   / \\  |
        |
============
    '''
]).


%Affiche une figure pour chaque indice
afficher_pendu(Indice) :-
    figures_pendu(ListeFigures),
    nth0(Indice, ListeFigures, Figure),
    write(Figure).

%la liste des mots pour le jeu
mots(['aimable','barracuda','bavard','chercheur','cocorico','dernier','egalite','fanfaronner','garage','hasard','huluberlu','infini','moitie','nulle','present','rare','savant']).
contextes(aimable, 'une personne agréable avec les autres').

%le nombre maximal d'erreurs
nombre_erreurs_max(6).

%L'alphabet pour ne plus retaper tout après
alphabet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

% choisit un mot aléatoirement dans la liste des mots pour le jeu
choisit_mot(X):- mots(Liste), length(Liste, A), Ind is random(A), nth0(Ind, Liste, X).

% Vérifie si X est dans un mot donné
est_dans(Mot, X):- atom_chars(Mot, L), member(X, L),!.


% Demande à l'utilisateur de donner une proposition et retourne cette
% proposition
demander_lettre(Choix,Props) :-
    repeat,
    write('Entrez une lettre : '),
    read_line_to_codes(user_input, Input),
    atom_codes(AtomChoix, Input), input_lettre(AtomChoix,Props),!, Choix= AtomChoix.


% verifie les conditions que doit remplir la proposition de
% l'utilisateur avant de le retourner
% Vérifie la longueur du choix de l'utilisateur et échoue quand ce n'est
% pas le cas
input_lettre(X,P):- not(atom_number(X,A)),not(est_dans(P,X)), not(number(A)), atom_chars(X, A), length(A, R), R>1, write('Proposez une seule lettre s''il vous plaît'), nl, fail.

%échoue quand le choix de l'utilisateur est un nombre
input_lettre(X,P):- atom_number(X, A), number(A),not(est_dans(P,X)), write('Ce n''est pas une lettre'),nl, fail.

%vérifie si le choix est dans l'alphabet et si c'est le cas ça réussit
input_lettre(X,P):- alphabet(Y), member(X,Y),not(est_dans(P,X)).

%vérifie si l'utilisateur l'a déjà proposé avant
input_lettre(X, Props):- est_dans(Props, X), write('Vous avez déjà proposé cette lettre'), nl,fail.


%Affiche un élément d'une liste
affiche_elt([]):- nl.
affiche_elt([X|R]):- write(X), write(' '), affiche_elt(R).

% Affiche les erreurs i.e les lettres qui ne sont pas le mot secret de
% l'utilisateur
affiche_erreurs([]).
affiche_erreurs([X|R]):- write('Erreurs :'),nl, write(' '),
 affiche_elt([X|R]),!.

affiche_erreur(MotSecret, X):- not(member(X, MotSecret)), write('Erreurs:'), nl, write(' '), write(X),!.

% affiche les lettres qui sont correctes dans la proposition de
% l'utilisateur
afficher_correctes([], []):-nl.

afficher_correctes([LettreSecret|ResteMotSecret], Props) :-

    member(LettreSecret, Props),
    write(LettreSecret), write(' '),
    afficher_correctes(ResteMotSecret, Props),!.

afficher_correctes([LettreSecret|ResteMotSecret], Props) :-
    not(member(LettreSecret, Props)),
    write('_ '), write(' '),
    afficher_correctes(ResteMotSecret, Props),!.
%retourne les propositions de l'utilisateur


affiche_chars(_, [], []).
affiche_chars(Props, [Char|Chars], [Char|ResChars]) :-
    member(Char, Props), !,
    affiche_chars(Props, Chars, ResChars).
affiche_chars(Props, [_|Chars], ['_'|ResChars]) :-
    affiche_chars(Props, Chars, ResChars).

% Affiche la proposition de l'utilisateur si ça se trouve dans le mot
% secret
% Définition du prédicat affiche_mot/2
affiche_mot(_, [], []).
affiche_mot(Props, [Lettre|ResteMotSecret], Affiche) :-
    member(Lettre, Props),
    Affiche = [Lettre|ResteAffiche],
    affiche_mot(Props, ResteMotSecret, ResteAffiche),!.

affiche_mot(Props, [_|ResteMotSecret], Affiche) :-
    Affiche = ['_'|ResteAffiche],
    affiche_mot(Props, ResteMotSecret, ResteAffiche),!.


%vérifie si l'utilisateur a gagné ou pas
gagne(Props, Mot_secret):- atom_chars(Props, P),atom_chars(Mot_secret, M), affiche_chars(P, M, Y), not(member('_', Y)).

%fonction principal du jeu
fonction(Props, MotSecret, Res):-
    repeat,
    forall((Res=<6, \+  gagne(Props, MotSecret)), (demander_lettre(Lettre, Props), compteur(Lettre, Props, MotSecret, Res))).


function([Props|X], MotSecret, Res):-


compteur(Lettre,Props, MotSecret, Res):-
   not(est_dans(MotSecret, Lettre)),
   Res1 is Res+1,
   atom_chars(Lettre,L),
   atom_chars(Props, P),
   append(P,L, Nouv),
   affiche_erreurs(Nouv),
   afficher_pendu(Res1).

compteur(Lettre,Props, MotSecret, Res):-
     est_dans(MotSecret, Lettre),
     atom_chars(Lettre, L),
     atom_chars(Props, P),
     append(P,L, Nouv),
     atom_chars(MotSecret, M),
     afficher_correctes(M,Nouv),
     Res1 is Res,
     afficher_pendu(Res1).

play(Props, MotSecret, Res):-

    compteur(Props, MotSecret, Res).

compteurs(Lettre, MotSecret, Props, Res) :-
    (
        est_dans(MotSecret, Lettre)
        ->  atom_concat(Props, Lettre, NouveauProps),
            atom_chars(NouveauProps, NouvelleListe),
            afficher_correctes(MotSecret, NouvelleListe),
            Res1 is Res
        ;   Res1 is Res + 1,
            affiche_erreurs([Lettre|Props])
    ),
    afficher_pendu(Res1),
    nl,
    play(NouvelleListe, MotSecret, Res1).




% Prédicat principal
maine :-
    write('Bienvenue dans le jeu du pendu !'), nl, nl,
    afficher_pendu(0), nl, nl,
    write('Le jeu consiste à deviner un mot lettre par lettre. Le jeu n''accepte pas les lettres majuscules et vous avez 7 chances. Après ces 7 chances si vous n''avez pas trouvé le mot secret alors vous perdez.'), nl, nl,
    write('Bonne chance !'), nl, nl.




% Prédicat principal pour jouer
 main :-
    writeln("Bienvenue dans le jeu du pendu."),
    Props = "", % aucune proposition n'a été faite
    choisit_mot(MotSecret), % mot choisi aléatoirement
    Res = 0, % tentatives du joueur
    afficher_pendu(Res), % affiche la première figure du pendu
    jouer(Props, MotSecret, Res).

jouer(Props, MotSecret, Res) :-
    Res < 6,
    \+ gagne(Props, MotSecret),
    demander_lettre(Propo, Props),
    atom_concat(Props, Propo, NouvellesProps),
    (   est_dans(MotSecret, Propo)
    ->
        atom_chars(MotSecret, M),
        atom_chars(NouvellesProps, N),
        afficher_correctes(M, N)

    ;   atom_chars(NouvellesProps, N),
        affiche_erreurs(N),
        Res1 is Res + 1,
        afficher_pendu(Res1)
    ),
    jouer(NouvellesProps, MotSecret, Res1).

jouer(Props, MotSecret, _) :-
    gagne(Props, MotSecret),
    format("Bravo ! Vous avez gagné. Le mot était ~w.~n", [MotSecret]).

jouer(_, MotSecret, Res) :-
    Res=6,
    format("Pas de chance ! Vous avez perdu. Le mot était ~w.~n", [MotSecret]),
    afficher_pendu(Res).


afficherr_mot(MotSecret, Correctes) :-
    atom_chars(MotSecret, Chars),
    maplist(afficherr_lettre(Correctes), Chars),
    nl.
afficherr_lettre(Correctes, Lettre) :-
    member(Lettre, Correctes),
    write(Lettre),
    write(' '),
    !.
afficherr_lettre(_, '_') :-
    write('_ ').
