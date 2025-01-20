
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
nombre_erreurs_max(7).

% choisit un mot aléatoirement dans la liste des mots pour le jeu

% choisir_mot/1
choisir_mot(Mot) :-
    mots(Liste), % Supposons que vous avez une liste de mots définie quelque part
    length(Liste, Longueur),
    random(0, Longueur, Indice),
    nth0(Indice, Liste, Mot).

% est_dans/2
est_dans(Mot, Car) :-
    sub_atom(Mot, _, 1, _, Car).

% input_lettre/2
input_lettre(Props, Lettre) :-
    repeat,
    write('Proposez une lettre : '),
    read(Lettre),
    (   length(Lettre, 1) -> true
    ;   write('Proposez une seule lettre, s\'il vous plaît\n'),
        fail
    ),
    (   est_dans(Props, Lettre) -> write('Vous avez déjà proposé cette lettre.\n'), fail
    ;   est_lettre(Lettre) -> true
    ;   write(Lettre), write(' n\'est pas une lettre.\n'),
        fail
    ).

% dessine_pendu/1
dessine_pendu(N) :-
    figures_pendu(Liste),
    nth0(N, Liste, Figure),
    write(Figure).

% affiche_erreurs/1
affiche_erreurs(Erreurs) :-
    write('Erreurs: '),
    maplist(write, Erreurs),
    nl.

% affiche_correctes/2
affiche_correctes(Correctes, MotSecret) :-
    atom_chars(MotSecret, MotChars),
    maplist(affiche_lettre(Correctes), MotChars),
    nl.

affiche_lettre(Correctes, Lettre) :-
    (   member(Lettre, Correctes) -> write(Lettre)
    ;   write('_')
    ),
    write(' ').

% gagne/2
gagne(Props, MotSecret) :-
    atom_chars(MotSecret, MotChars),
    maplist(member, MotChars, Props).

% main/0
main :-
    writeln('Bienvenue dans le jeu du pendu.'),
    choisir_mot(MotSecret),
    writeln(''),
    resoudre_pendu(MotSecret, '', 0).

resoudre_pendu(MotSecret, Props, Res) :-
    (   Res < 10,
        \+ gagne(Props, MotSecret) ->
        input_lettre(Props, Lettre),
        (   est_dans(MotSecret, Lettre) ->
            affiche_correctes(Props, MotSecret),
            resoudre_pendu(MotSecret, [Lettre|Props], Res)
        ;   Res1 is Res + 1,
            dessine_pendu(Res1),
            affiche_erreurs([Lettre|Props]),
            resoudre_pendu(MotSecret, [Lettre|Props], Res1)
        )
    ;   (   gagne(Props, MotSecret) ->
            writeln('Bravo! Vous avez gagné'),
            writeln(MotSecret)
        ;   writeln('Pas de chance! Vous avez perdu'),
            writeln(MotSecret)
        )
    ).


%Vérifie si ce que l'utilisateur a proposé est bien une lettre
%analyse(X):- alphabet(Y), member(X, Y).
% analyse(X):- number(X), alphabet(Y), not(member(X, Y)), write('Ce
% n''est pas une lettre').
% analyse(X):- length(X, Z), Z>1, write('Proposez une seule lettre s''il
% vous plaît'). analyse(X):- alphabet([Y|R]), X\==Y, write('Veuillez
% saisir une lettre'), nl, fail, alphabet(R). analyse(X):- atom_chars(X,
% Y), length(Y, Z), Z>1, write('Proposez une seule lettre'), nl, fail.
% analyse(X):- write('Vous avez déjà proposé cette lettre'), nl, fail.

analyse(o, 1).
analyse(n, 0).
analyse(_,_) :- write('Error').
%Retourne les propositions de l'utilisateur pour trouver le mot secret
affiche(Props, Mot_secret, Y):- est_dans(Mot_secret, Z), Z==Props, Y=Props.
affiche(Props, Mot_secret, Y):- est_dans(Mot_secret, Z), Z\== Props, Y='_'.

ask_user :-
   format("You must answer 'yes' to accept this offer one cannot refuse!~n"),
   read_line_to_string(user_input,S1),
   string_lower(S1,S2),
   (
      member(S2,["yes","1","ok","y","ja","oui"])
      -> format("You may proceed, good citizen.~n")
      ;  format("I am sorry to hear that.~n"), fail
   ).

%retourne ce que l'utilisateur a proposé


demande_lettre(Lettre) :-
    repeat,
    write('Proposez une lettre: '),
    read_line_to_string(user_input, Valeur),

   % atom_length(Valeur, 1), % Vérifie si la valeur est un atome d'une seule lettre
    atom_chars(Valeur, [Lettre]), % Extrait la lettre de l'atome
    %char_type(Lettre, lower), % Vérifie si la lettre est en minuscule
    !.

input_lettre(Props):- atom_chars(Props, L), demande_lettre(Lettre), member(Lettre, L), write("Vous avez déjà proposé cette lettre"),!.

:- initialization(main).
main(X) :-
    write('Entrez quelque chose : '),
    read_line_to_codes(user_input, Choix),
    atom_codes(AtomChoix, Choix), X=AtomChoix,
    (verifier_choix(AtomChoix) ->
        write('Vous avez choisi : '),
        write(AtomChoix);
        write('Ce n\'est pas une seule lettre.')
    ).

verifier_choix(Choix) :-
    atom_length(Choix, 1).

%:- initialization(main).
%main :-
%    demande_lettre(Choix),
 %   write('Vous avez choisi : '),
  %  write(Choix).

% Prédicat pour demander à l'utilisateur d'entrer une lettre
demander_lettre(Choix) :-
    write('Entrez une lettre : '),
    read_line_to_codes(user_input, Input),
    atom_codes(AtomChoix, Input),
    (   est_lettre(AtomChoix)
    ->  Choix = AtomChoix
    ;   write('Ce n\'est pas une seule lettre. Veuillez entrer une seule lettre.'), nl,
        demander_lettre(Choix) % Appel récursif pour redemander une lettre
    ).

% Vérifie si le choix est une seule le
askLettre(X):- write('Entrez une lettre : '), read_line_to_codes(user_input, Input), atom_codes(AtomChoix, Input), X=AtomChoix.

ask_lettre(X):- write('Entrez une lettre : '), read_line_to_codes(user_input, Input), atom_codes(AtomChoix, Input), est_lettre(AtomChoix), X= AtomChoix.

ask_lettre(X):- write('Entrez une lettre : '), read_line_to_codes(user_input, Input), atom_codes(AtomChoix, Input), not(est_lettre(AtomChoix)), write('Ce n\'est pas une lettre. Veuillez proposez une seule lettre s\'il vous-plaît'),nl, ask_lettre(X).


:- initialization(main).
main :-
    demande_lettre(Choix),
    write('Vous avez choisi : '),
    write(Choix).

% Prédicat pour demander à l'utilisateur d'entrer une lettre
demand_lettre(Choix) :-
    write('Entrez une lettre : '),
    read_line_to_codes(user_input, Input),
    atom_codes(AtomChoix, Input),
    (   est_lettre(AtomChoix)
    ->  Choix = AtomChoix
    ;   write('Ce n\'est pas une seule lettre. Veuillez entrer une seule lettre.'), nl,
        demand_lettre(Choix) % Appel récursif pour redemander une lettre
    ).

% Vérifie si le choix est une seule lettre
est_lettre(L) :-
    atom_length(L, 1),
    !. % Arrête la recherche dès qu'une lettre est trouvée
est_lettre(_) :-
    write('Ce n\'est pas une seule lettre. Veuillez entrer une seule lettre.'), nl,
    demande_lettre(_). % Redemande une lettre si ce n'est pas une seule lettre
