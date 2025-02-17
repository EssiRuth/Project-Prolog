afficher_contenu([]) :-
    write('|'), nl.
afficher_contenu([Symbole|Reste]) :-
    write('| '),
    write(Symbole),
    afficher_contenu(Reste).

ligne_exemple(N) :-
    length(Liste, N),
    maplist(=(o), Liste), % Cr�e une liste remplie de 'o'
    afficher_ligne(N),
    afficher_contenu(Liste),
    afficher_ligne(N).

afficher_ligne(0) :-
    write('+'), nl.
afficher_ligne(N) :-
    N > 0,
    write('+--'),
    NMoinsUn is N - 1,
    afficher_ligne(NMoinsUn).

demander_prop(ChoixRond):-
    repeat,
    write('Combien rond veux-tu supprimer: '),
    read_line_to_codes(user_input, Input),
    atom_codes(AtomChoix, Input),input_prop(AtomChoix),!, ChoixRond= AtomChoix.

demander_props(ChoixCol):-
    repeat,
    write('Combien veux-tu enlever? : '),
    read_line_to_codes(user_input, Input),
    atom_codes(AtomChoix, Input),input_props(AtomChoix),!, ChoixCol= AtomChoix.

%input_props(Choix):- member(Choix, ['1','2','3','4']).
% input_props(Choix):- not(member(Choix, ['1','2','3','4'])),
% write('Proposez un chiffre entre 1,2,3 et 4 s''il vous pla�t'), nl,
% fail.

input_prop(Choix):- member(Choix, ['1','2','3']).
input_prop(Choix):- not(member(Choix, ['1','2','3'])), write('Proposez un chiffre entre 1,2,3 et 4 s''il vous pla�t'), nl, fail.

replace([_|R], 1, X, [X|R]).
replace([X|L], A, B,[X|R]):- A>1, N is A-1, replace(L, N, B, R).


replacer(R, A, B, C, L):- replace(R, A, x, L1), replace(L1, B, x, L2), replace(L2,C,x, L).
replacer(R, A, B, L):- replace(R, A, x, L1), replace(L1, B, x, L).
replacer(R, A, L):- replace(R, A, x, L).

est_deja_enleve(R):-

% DIVERS
longueurliste([],0):-!.
longueurliste([_|L],N):- longueurliste(L,M),N is M+1.
invers(X,Y,Z):-W is X-1, Z is Y-W.
elementliste([X|_],1,Resultat):-!,Resultat=X.
elementliste([_|Reste],Numero,Resultat):-N is Numero-1,elementliste(Reste,N,Resultat).
% Sort l'�l�ment de position Num�ro de la Liste.
plateau_vide([]):-!.
plateau_vide([X|R]):-X=0,plateau_vide(R).
% V�rifie que le plateau est vide donc partie finie.

% Finalement inutilis�
%binaire(0,[0]):-!.
%binaire(1,[1]):-!.
%binaire(D,B):- X is rem(D,2),Y is D // 2, binaire(Y,B2), append(B2,[X],B).
% Pour convertir un chiffre d�cimal en son �quivalent binaire sous forme de liste.
%decimal([],0):-!.
%decimal([X|B],D):-longueurliste([X|B],L),N is L-1, Y is X*2^N, decimal(B,Z), D is Y+Z.
% Pour convertir une liste binaire en nombre decimal (utiliser le pr�dicat "binaire" en inversant les inconnues ne marche pas car trop d'arguments non instanci�s dans ce sens.)



% MISE EN PLACE
allumettes(0):-!.
allumettes(N):-N>0,write('|'),N1 is N-1, allumettes(N1).
afficher_jeu(L):- longueurliste(L,X),afficher_jeu(L,X).
afficher_jeu([],_):-!.
afficher_jeu([N|L],T):-longueurliste([N|L],X),invers(X,T,Y),write(Y),write('='),write(N),tab(2),allumettes(N),nl,afficher_jeu(L,T).
affiche:-writeln(' '),plateau(P), afficher_jeu(P),writeln(' ').
% Permet d'avoir un visuel des diff�rents tas d'allumettes.

demarre:- A is random(11),B is random(11),C is random(11),D is random(11),E is random(11), asserta(plateau([A,B,C,D,E])).
% Cr�e le plateau de jeux de base avec 5 tas contenant chacun un nombre al�atoire d'allumettes entre 0 et 10.
nettoie:-retractall(plateau(_)).
% Efface le plateau existant.

plateau([[o,o,o,o,o,o,o]]).

% MOUVEMENTS DE JEU
% JOUEUR
enleve:-write('Indiquez la ligne � modifier, entre 1 et '),plateau(P),longueurliste(P,T),write(T),
					write(' : '),read(A),0<A,A=<T,!,write(' Indiquez le nombre d allumettes � enlever : '),read(B),enleve(A,B,P,R),!,enleve(R).
enleve:-writeln('Cette ligne n existe pas, veuillez recommencer.'),enleve.
enleve(R):-member(11,R),!,enleve.
% permet de recommencer si le nombre donn� �tait sup�rieur au nombre restant d'allumettes, tout en permettant que la fonction retourne true quand m�me.
enleve(R):-retract(plateau(_)),asserta(plateau(R)).
enleve(1,N,[X|_],Z):- Y is X-N, Y<0,!,
					writeln('Vous essayez d enlever un trop grand nombre d allumettes, veuillez recommencer.'),append([11],_,Z).
enleve(1,N,[X|Xs],R):- Y is X-N, Y>=0,!,R=[Y|Xs].
enleve(L,N,[X|Xs],R):- L2 is L-1,enleve(L2,N,Xs,Rs),append([X],Rs,R).
% Permet d'enlever le nombre que l'on souhaite d'allumettes sur une ligne, tant qu'il y a suffisemment d'allumettes et que la ligne existe.

% ORDINATEUR
test_gagnant:-plateau(P),test_gagnant(P,X),X==0.
test_gagnant([],0):-!.
test_gagnant([X|Xs],R):- test_gagnant(Xs,Rs),R is Rs xor X.
% Si le test renvoie true, le joueur qui a mis le plateau dans cette situation est en position gagnante.

ordi:-not(test_gagnant),!,plateau([X|P]),ordi([X|P],L,N,X,[]),enleve(L,N,[X|P],R),enleve(R).
ordi:-test_gagnant,!,plateau(P),ordibete(P,L,N),enleve(L,N,P,R),enleve(R).
ordi([0,X|Xs],L,N,Element,Liste):-!,append(Liste,[Element],L2),ordi([X|Xs],L,N,X,L2).
ordi([X|Xs],L,N,Element,Liste):- X>0,Y is X-1,append(Liste,[Y|Xs],Test),test_gagnant(Test,0),!,longueurliste([Y|Xs],M),plateau(P),
				longueurliste(P,T),invers(M,T,L),N is Element-Y.
ordi([X|Xs],L,N,Element,Liste):- X>0,Y is X-1,ordi([Y|Xs],L,N,Element,Liste).
% Si l'ordinateur en a la possibilit�, il joue de fa�on � se retrouver en position gagnante. Pour cela il faut que le ou exclusif bit � bit du nombre d'allumettes de chaque ligne fasse 0.
ordibete(Liste,Ligne,Nombre):-longueurliste(Liste,Taille),Lignetest is random(Taille)+1,ordibete(Lignetest,Ligne),!,
				elementliste(Liste,Ligne,Y),Nombre is random(Y)+1.
ordibete(Lignetest,Lignevalide):-plateau(P),elementliste(P,Lignetest,Y),Y\==0,!,Lignevalide=Lignetest.
ordibete(Lignetest,Lignevalide):-plateau(P),elementliste(P,Lignetest,Y),Y==0,!,longueurliste(P,Taille),Lignetest==Taille,!,
				ordibete(1,Lignevalide).
ordibete(Lignetest,Lignevalide):-plateau(P),elementliste(P,Lignetest,Y),Y==0,!,longueurliste(P,Taille),Lignetest<Taille,!,
				Lignetest2 is Lignetest+1,ordibete(Lignetest2,Lignevalide).
% Si l'ordinateur ne peut pas se mettre en position gagnante, il joue au hazard.



% D�ROULEMENT PARTIE


jeu:-writeln(' '),writeln('Bienvenue sur le jeu de Nim !'),writeln('Voulez-vous jouer avec le plateau de jeu de base, un plateau al�atoire ou un plateau enti�rement au choix ?'),
				writeln('Pour celui de base taper "basique.", pour l al�atoire tapez "random.",'),
				writeln('sinon taper votre liste de nombre d allumettes sous la forme "[X,X,X,...].".'),
				writeln('Pour un rappel des r�gles tapez "r�gles.". Pour quitter tapez "quit."'),read(X),jeu(X).
% Choix du mode de jeu avec un plateau de jeu random.
jeu(basique):-!, nettoie,asserta(plateau([1,3,5,7])),writeln('Voulez-vous jouer contre un autre joueur ou contre l ordinateur ?'),
				writeln('Pour jouer contre un joueur tapez "joueur.", sinon tapez "ordi.".'),
				writeln('Vous pouvez toujours quittez ou voir les r�gles.'),read(X),jeu(X).
% Choix du mode de jeu avec le plateau de jeu de base.
jeu(random):-!, nettoie,demarre,writeln('Voulez-vous jouer contre un autre joueur ou contre l ordinateur ?'),
				writeln('Pour jouer contre un joueur tapez "joueur.", sinon tapez "ordi.".'),
				writeln('Vous pouvez toujours quittez ou voir les r�gles.'),read(X),jeu(X).
% Choix du mode de jeu avec un plateau de jeu random � 5 rang�es.
jeu(quit):-!,nettoie.
jeu(r�gles):-!,writeln(' '),writeln('R�gles : Le jeu de Nim est jou� sur un plateau o� sont dispos�es des rang�es.'),
				writeln('Chaque rang�e est compos�es d entre 0 et 10 allumettes.'),
				writeln('Chaque joueur � tour de r�le va devoir prendre au moins 1 allumette dans une des rang�es.'),
				writeln('Le joueur qui prends la derni�re allumette a gagn�.'),
				writeln('Si vous avez compris les r�gles tapez "basique." pour jouer avec le plateau de base,'),
				writeln('tapez "random." pour jouer avec un plateau al�atoire, ou bien tapez votre propre liste de nombre d allumettes.'),
				writeln('Sinon tapez "quit." pour quitter.'),
				read(X),jeu(X).

jeu(joueur):-plateau(P),plateau_vide(P),!,affiche,writeln('Le plateau est vide, joueur 2 a gagn�,F�licitation !'),
				write('Pour recommencer tapez "jeu."'),nettoie.
jeu(joueur):-!,affiche,writeln('Joueur 1 � vous de jouer !'),enleve,jeu(joueur2).
jeu(joueur2):-plateau(P),plateau_vide(P),!,affiche,writeln('Le plateau est vide, joueur 1 a gagn�, F�licitation !'),
				write('Pour recommencer tapez "jeu."'),nettoie.
jeu(joueur2):-!,affiche,writeln('Joueur 2 � vous de jouer !'),enleve,jeu(joueur).
% Pour une partie � deux joueurs.

jeu(ordi):-!, writeln(' '),writeln('Souhaitez-vous commencer ou laissez l ordinateur jouer en premier ?'),
				writeln('Pour commencer en premier tapez "moi.", sinon tapez "ai."'),
				read(X),jeu(X).
jeu(ai):-plateau(P),plateau_vide(P),!,affiche,writeln('Le plateau est vide, vous avez gagn�, F�licitation !'),
				write('Pour recommencer tapez "jeu."'),nettoie.
jeu(ai):-!,affiche,writeln('Tour de l ordinateur'),ordi,jeu(moi).
jeu(moi):-plateau(P),plateau_vide(P),!,affiche,writeln('Le plateau est vide, l ordinateur a gagn�, Dommage !'),
				write('Pour recommencer tapez "jeu."'),nettoie.
jeu(moi):-!,affiche,writeln('C est � vous de jouer !'),enleve,jeu(ai).
% Pour une partie contre l'ordinateur.
jeu(L):-nettoie,asserta(plateau(L)),writeln('Voulez-vous jouer contre un autre joueur ou contre l ordinateur ?'),
				writeln('Pour jouer contre un joueur tapez "joueur.", sinon tapez "ordi.".'),
				writeln('Vous pouvez toujours quittez ou voir les r�gles.'),read(X),jeu(X).
% Choix du mode de jeu avec un plateau de jeu donn� en argument.
