:- dynamic attributes/1.
/* lecture d'un fichier .dot */
read_info_lignes(I, Str, att_graph(S, A)) :-
    /* Traitement début du fichier, extraction de la liste des labels,
    puis des labels associés à chaque sommet */

    /*==:read_string: lit une chaine de caractères depuis str et la met dans string en ignorant les \r et lit jusqu a la fin de la ligne */

    read_string(Str, "\n", "\r", _, String),
    % write('Lecture ligne '),write(I),write('  = '),writeln(String),
      /*==: split_string: (recupère l'information d'un sommet qui commence de = à ; et la met dans une liste)*/

    split_string(String, "=", " ;", L),
      /*write("avant = "),writeln(String),
     write("liste extraite = "),writeln(L),*/
    extract_info_ligne(I, L, Sommet),
    NI is I+1,
    (Sommet = [] ->
        % ligne de début de fichier, on passe
        read_info_lignes(NI, Str, att_graph(S, A));
        % ligne contenant un sommet et ses attributs
        (Sommet = sommet(X, Y) ->
            read_info_lignes(NI, Str, att_graph(NS, A)),
            S = [sommet(X, Y)|NS];
            % ligne décrivant une arête
            (Sommet = arete(X, Y) ->
                read_info_lignes(NI, Str, att_graph(S, NA)),
                A = [arete(X, Y)|NA];
                % lecture de la fin du graphe
                (Sommet = fin ->
                    S = [], A = []
                )
            )
        )
    ).

/*== sub_string: recupere une sous chaine ss à partir de s*/
msubstring(S, SS) :- sub_string(S, _, _, _, SS).

/* extract_info_ligne(+NL,+L,-Term */
/* NL : numéro de ligne, L la chaîne de caractères lue, Term est le terme utile extrait */

/* Lecture des lignes de début de fichier, aucune information utile, on rend [] */
extract_info_ligne(1, L, []) :-
    member("Graph {", L).
extract_info_ligne(2, L, []) :-
    member(E, L),msubstring(E, "labelloc").
extract_info_ligne(3, L, []) :-
    member(E, L), msubstring(E, "fontsize").
/* Ici on lit la liste des labels admissibles pour les sommets, ils sont assertés dans save_labels (variable globale) */
extract_info_ligne(4, L, []) :-
    member(E, L), msubstring(E, "label"),
    save_labels(L).
/*  on lit } : fin de graphe */
extract_info_ligne(_, L, fin):-
    member("}", L).


/* Lecture des lignes contenant un identifiant de sommets  aucune information utile, on rend [] */
extract_info_ligne(N, L, Res) :-
    N>4,
    member(E, L),
    (msubstring(E, "label")->
        % on est dans le cas des labels de noeuds cette fois ci
        parse_labels(L, Res);
        (msubstring(E, "--")->
            parse_aretes(L, Res)
        )
    ).

save_labels([_, T]) :-
    split_string(T, ",", ", []\"", [_|L]), maplist(string_to_atom, L, LV),
    /*write("Liste valeurs attributs "), writeln(LV),*/
    assertz(attributes(LV)).

parse_labels([_, T], Term) :-
    split_string(T, ",[]", " \t\s\n[]\"", L),
    maplist(string_to_atom, L, LV),
    LV = [HLV|TLV], Term = sommet(HLV,TLV).
    /*write('Sommet extrait = '), writeln(Term)*/

parse_aretes([T], Term) :-
    split_string(T,"-", " -[]\t", A),
    maplist(string_to_atom, A, AL),
    Term =.. [arete|AL].
  /*  write('Arete extraite '), writeln(Term).*/

/* read_dot_file(+NomFichier) avec NomFichier : chaine de caractères */
/* read_dot_file('mougel_bis.dot'). */


read_dot_file(NomF, att_graph(S,A)) :-
    retractall(attributes),
    open(NomF,read,Str),
    read_info_lignes(1,Str,att_graph(S,A)),
    close(Str).
    /*writeln(att_graph(S,A))*/
    /*write('Nb sommets = '),length(S,NS),writeln(NS),
    write('Nb aretes = '),length(A,NA),writeln(NA)*/

/*
etiquette(+sommet(_, L), -Etiq)
Recuperer l etiquette d un sommet
Etiq contiendra l etiquette d un sommet.
exemple:
      ?- etiquette(sommet(a,[rock, folk, jazz]), Etiq).
      Etiq = [rock, folk, jazz].
*/
etiquette(sommet(_, L), Etiq) :- L = Etiq.


/*
intersection(+L1, +L2, -L3)
Intersection de deux listes L1 et L2 et le resultat dans L3

Exemple:
?- intersection([rock, jazz, pop], [pop, rock, blues, folk], L).
L = [rock, pop].

Source: http://eclipseclp.org/doc/bips/lib/lists/intersection-3.html
*/
intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-  memberchk(Head, L2), !, L3 = [Head|L3tail], intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :- intersection(L1tail, L2, L3).

/*acclen(+L, +Acc, -Long)
Prédicat qui calcule la longuer d une liste avec un accumulateur, le resultat est mis dans Long
Exemple:

?- acclen([rock, jazz, pop], 0, X).
X = 3.
*/

acclen([],Acc, Length) :- Length = Acc.
acclen([_|L], OldAcc, Length) :-  NewAcc is OldAcc + 1, acclen(L, NewAcc, Length).

/*
contientMotif(+Etiq, +Motif)
Ce prédicat renvoi true si l'etiquette contient tout le motif, False sinon.
Il commence par calculer l intersection entre une liste 'Etiquette' et une liste 'Motif' dans une autre liste L,
il renvoie true si la longueur de la liste resultat est egale a la longueur de la liste motif.

Exemple:

?- contientMotif([rock, folk, jazz],[jazz, rock]).
true.

?- contientMotif([rock, folk, jazz],[jazz, rock, pop]).
false.
*/
contientMotif(Etiq, Motif) :- intersection(Etiq, Motif, L), acclen(L, 0, LongL), acclen(Motif, 0, LongM), LongL is LongM.


/*
recupererSommetMotif(+Sommet,+Motif,-ListeSommetM, -SommetM)
ce predicat recupere tous les sommets d une liste qui ont un motif donne
exemple :
      ici le motif est [rockn jazz]

?- recupererSommetMotif([sommet(a,[rock, jazz]), sommet(b,[jazz, pop, blues]), sommet(c, [jazz, pop, rock])],[rock, jazz],L, L2).
L = [a, c],
L2 = [sommet(a, [rock, jazz]), sommet(c, [jazz, pop, rock])] ;

*/

recupererSommetMotif([],_, [], []).
recupererSommetMotif([Element|Re],Motif,[X|L], [Element|L2]):-
                        Element = sommet(X,Etiq),
                        contientMotif(Etiq, Motif),
                        recupererSommetMotif(Re, Motif, L, L2).

recupererSommetMotif([Element|Re],Motif,L, L2):-
                      Element = sommet(_,Etiq),
                      \+contientMotif(Etiq, Motif)->
                      recupererSommetMotif(Re, Motif,L, L2).


split(L, L).
split([_|Q], L2) :- split(Q, L2).

/*
comb(+N,+L,-L3)
Ce predicat recupere dans L3 les combinaisons de longeur N d une liste L une par une.

Exemple:

?- comb(2,[a,b,c,d],L).
L = [a, b] ;
L = [a, c] ;
L = [a, d] ;
L = [b, c] ;
L = [b, d] ;
L = [c, d] ;
false.

*/

comb(0,_,[]).
comb(N,L,[T|L3]) :- N>0, split(L, [T|L2]), N1 is N-1, comb(N1,L2,L3).

/*Pour pouvoir determiner les aretes associes aux sommets ayant un motif donne, on souhaite faire la combinaison des sommets qui satisfont le Motif
dans une liste, et verifier pour chaque element(arete) si il appartient a la liste des aretes A du graphe
Exemple:
Pour le motif [rock, jazz], les sommets qui satisfont ce motif sont [a, b, c, d, g, p],
Les combinaisons des sommets sont: [(a,b), (a,c), (a,d), (a,g), (a,p), (b,c), (b,d), (b,g), (b,p), (c,d), (c,g), (c,p), (d,g), (d,p), (g,p)] */

/*
combinaisonSommet(+N, +Sommet, -L)
Ce predicat recupere toutes les combinaisons des sommets de taille N dans la liste L en utilisant findall qui reuni tous les resultats dans une liste

Exemple:

?- combinaisonSommet(2, [a,b,c,d], L).
L = [[a, b], [a, c], [a, d], [b, c], [b, d], [c, d]].
*/

combinaisonSommet(N, Sommet, L):- findall(X,comb(N,Sommet,X) ,L).

/*
listeAreteGraphe(+Arete,-L)
Ce predicat prend en parametre une liste d aretes [arete(a,b), arete(a,c), arete(b,d)], et renvoie dans la lite L, la liste des listes des aretes

Exemple:

?- listeAreteGraphe([arete(a,b), arete(a,c), arete(b,d)], L).
L = [[a, b], [a, c], [b, d]] ;
false.

*/
listeAreteGraphe(A,[]):- A = [].
listeAreteGraphe(Arete, [[X,Y]|L2]) :- Arete = [arete(X,Y)|L], listeAreteGraphe(L,L2 ).


/*

recupererAreteMotif(+Sommet,+Arete, +Motif, -L )
Ce predicat prend en parametre une liste de sommets de la forme [sommet(a,[c,b]), sommet(b,[g,p])], ainsi qu une liste d aretes de la forme
[arete(c,d), arete(a,c), arete(b,c)], ainsi q un motif avec lequel il selectionne l ensembledes sommets qui satisfont ce motif, et renvoie
une liste contenant les aretes entre les sommets qui satisfont le motif en tenant en compte des aretes du graphe de depart.

Il fait appel au predicat recupererSommetMotif(Sommet,Motif,ListeSommetM, SommetM) qui recupere dans une liste ListesommetM, les sommets qui satisfont le motif,
puis fait la combinaison de ces sommets dans une liste L1 en faisant appel a combinaisonSommet(2, SommetM, L1), ensuite, il fait appel au predicat
  listeAreteGraphe(Arete, L2) qui recupere dans une liste L2 la liste des aretes du graphe, et enfin, il fait l intersection entre L1 et L2
dans une liste L qui represente les aretes entre les sommets qui satisfont le motif donne en parametre.

Exemple:

?- recupererAreteMotif([sommet(a,[k,l]), sommet(b,[l,m]), sommet(c,[k,m,l]), sommet(d,[l,p,m])],[arete(c,d), arete(a,c), arete(b,c)],[l,m],L).
L = [[b, c], [c, d]] ;

*/


recupererAreteMotif(Sommet,Arete, Motif, L):-
                          recupererSommetMotif(Sommet,Motif,ListeSommetM, _),
                          combinaisonSommet(2, ListeSommetM, L1),
                          listeAreteGraphe(Arete, L2),
                          intersection(L1, L2, L).


/*
transformerArete(+ListeArete, -L).

Ce predicat prend en parametre une liste ListeArete de couples de sommets(aretes) et renvoie dans L une liste d aretes de la forme arete(a,b),

Exemple:

?- transformerArete([[a,b],[c,b]],L).
L = [arete(a, b), arete(c, b)].

*/

transformerArete(ListeArete,[]):- ListeArete = [].
transformerArete([[X,Y]|T], [arete(X,Y)|L]) :- transformerArete(T,L).

/*

sousGrapheMotif(+NomF, +Motif, -ListeSommetM, -ListeAreteM, -SommetM, -AreteM)

Ce predicat prend en parametre le nom du fichier ou se trouvent les sommets et aretes du graphe ainsi qu un motif sous forme d une liste, et renvoie dans
ListeSommetM: La liste des sommets(sans etiquettes) dont l etiquette contient le motif donne en parametre
ListeAretesM: la liste des couples de sommets (aretes) entre les sommets qui satisfont le motif en tenant en compte des aretes de depart du graphe.
SommetM: Une liste de sommets avec leurs etiquettes de la forme [sommet(b, [rock, folk, blues, jazz]), sommet(e, [rock, folk, blues])] dont l etiquette
          contient le motif donne en parametre
AreteM: Une liste d aretes de la forme [arete(e, f), arete(n, r)] entre les sommets qui satisfont le motif en tenant en compte des aretes de depart du graphe.

Exemple:

?- sousGrapheMotif('mougel_bis.dot', [folk, blues], ListeSommetM, ListeAreteM, SommetM, AreteM).
Le sous graphe induit par les sommets dont l'étiquette contient le motif est:

att_graph([sommet(b,[rock,folk,blues,jazz]),sommet(e,[rock,folk,blues]),sommet(f,[folk,blues]),sommet(i,[folk,blues]),
sommet(n,[rock,folk,pop,blues]),sommet(r,[rock,folk,pop,blues])],[arete(e,f),arete(n,r)])
ListeSommetM = [b, e, f, i, n, r],
ListeAreteM = [[e, f], [n, r]],
SommetM = [sommet(b, [rock, folk, blues, jazz]), sommet(e, [rock, folk, blues]), sommet(f, [folk, blues]), sommet(i, [folk, blues]),
sommet(n, [rock, folk, pop|...]), sommet(r, [rock, folk|...])],
AreteM = [arete(e, f), arete(n, r)] ;

*/
sousGrapheMotif(NomF, Motif, ListeSommetM, ListeAreteM, SommetM, AreteM):-
                          read_dot_file(NomF, att_graph(Sommet,Arete)),
                          recupererSommetMotif(Sommet,Motif,ListeSommetM, SommetM),
                          recupererAreteMotif(Sommet,Arete, Motif,ListeAreteM),
                          transformerArete(ListeAreteM, AreteM),
                          write('\n Le sous graphe induit par les sommets dont l\'étiquette contient le motif est: \n\n'),
                          writeln(att_graph(SommetM,AreteM)).


/*
listeFirstElement(+Arete, -L, -L2).
Ce predicat prend en parametre une liste d aretes (une liste de listes), et renvoie une liste L qui contient le premier element de chaque liste
ainsi qu une liste l2 qui contient le second memebre de chaque liste.
Exemple:

?- listeFirstElement([ [a,b], [a,c], [c,d], [c,e],[d,l], [d,e]], L, L2).
L = [a, a, c, c, d, d],
L2 = [b, c, d, e, l, e].
*/
listeFirstElement([], [], []).
listeFirstElement([[X|T]|L], [X|L2], [Ts|L3]):- T = [Ts], listeFirstElement(L, L2, L3).


transfer(_,[],[],[]).
transfer(X,[Y|Xs],[Y|Xs],[]):- X\=Y.
transfer(X, [X|Xs],Ys,[X|Zs]):- transfer(X,Xs, Ys, Zs).

pack([],[]).
pack([X|Xs], [Z|Zs]):- transfer(X,[X|Xs], Ys, Z), pack(Ys, Zs).

/*
pack(+Liste, -L).
Ce predicat prend une liste en parametre et renvoie une liste de listes, chaque liste contient les elements egaux

Exemple:

?- pack([a,a,a,a,b,c,c,a,a,d,e,e],L).
L = [[a, a, a, a], [b], [c, c], [a, a], [d], [e, e]]
*/

uncode(L1, L2):- pack(L1,L), transform(L,L2).
/*
uncode(+Liste, -L).
Ce predicat prend en parametre une liste et renvoie une liste de liste, chaque liste contient un element de la liste ainsi que son occurence

Exemple:

?- uncode([a,a,b,b,b], L).
L = [[2, a], [3, b]]

*/

transform([],[]).
transform([[X|Xs]|Ys], [[N,X]|Zs]):- length([X|Xs],N), transform(Ys, Zs).

/*
sommetDegre(+L, +N, -L2).
Ce predicat prend en parametre une liste de liste, tel que chaque liste contient un element et son degre, et renvoie dans L2 la liste des listes
dont le degre est superieur ou egale a N.

Exemple:
?- sommetDegre([ [3,a], [2,b], [1,c], [4,d] ], 2, L).
L = [[3, a], [2, b], [4, d]] ;
*/

sommetDegre([], _,[]).
sommetDegre([[X|L]|T], N, [[X|L]|L2]):- X >= N, sommetDegre(T,N,L2).
sommetDegre([[X|_]|T], N, L2):- X < N, sommetDegre(T,N,L2).

/*
lesSommetsD(+L, -L2)
Ce predicat prend en parametre une liste de listes, chaque liste contient un sommet et son degre et renvoie une liste contenant les Sommets

Exemple:

?- lesSommetsD([[2,a], [3,b], [4,c]], L).
L = [a, b, c].*/

lesSommetsD([], []).
lesSommetsD([[_,X|_]|L], [X|L2]):- lesSommetsD(L, L2).

/*
secondEl(+X, +L, +L3, -L2)
Ce predicat prend en parametre un sommet et une liste d aretes ainsi qu une liste de sommet et renvoie la liste de listes tel que le premier
 element est egale au X donne e parametre en verifiant si l autre extrimite du sommet appartient a la liste de sommets L3.

Exemple:

?- secondEl(a, [[a,r],[a,c],[b,t],[a,u]], [a,b,c,t,r] ,L).
L = [[a, r], [a, c]] ;
false.
*/

secondEl(_, [],_,[]).
secondEl(X, [[Z,Y]|L], L3, [[Z,Y]|L2]):- X == Z, member(Y,L3), secondEl(X, L, L3, L2).
secondEl(X, [[Z,Y]|L], L3, L2):- X == Z, \+member(Y,L3), secondEl(X, L, L3, L2).
secondEl(X, [[Z,_]|L], L3, L2):- X \= Z, secondEl(X, L, L3, L2).

/*
aretesDegre(+Sommet, +Arete, +ListeSommet,-L)
Ce predicat prend en parametre un liste de sommets et une liste d aretes et renvoie dans L les aretes qui ont
comme exremite gauche l un des sommets present dans la liste Sommet.

Exemple:

?- aretesDegre([a,b], [[a,y],[a,c],[b,h],[g,h],[b,l],[a,d]], [a,b,y,g,d,l], L).
L = [[a, y], [a, d]] ;
L = [[b, l]] ;
false.

*/

aretesDegre([X|_],L1, L2, L3):- secondEl(X,L1, L2, L3).
aretesDegre([_|L],L1, L2, L3):- aretesDegre(L, L1, L2,L3).

/*
aretesDesSommets(+Sommet,+Arete,S, -L)
Ce predicat fait appel a findall pour recuperer toutes les aretes qui ont comme exremite l un des sommets present dans la liste Sommet

Exemple:

?- aretesDesSommets([a,b], [[a,y],[a,c],[b,h],[g,h],[b,l],[a,d]], [a,b,y,g,d,l], L).
L = [[[a, y], [a, d]], [[b, l]]].

*/
aretesDesSommets(L, L1, L2, R):- findall(X, aretesDegre(L,L1,L2,X), R).

/*il faut verifier que les seconds memebre des aretes appartiennt aux sommets lessommets */

/*
areteInverse(+L, -L2).
Ce perdicat prend en argument une liste d aretes L, et renvoie une liste avec les aretes inverses

Exemple:
?- areteInverse([[a,b], [c,d], [e,f]],L).
L = [[b, a], [d, c], [f, e]].

*/
areteInverse([],[]).
areteInverse([[X,Y]|L], [[Y,X]|L2]):- areteInverse(L, L2).

/*
conctListOfList(+L, -R).

Ce predicat prend en parametre une liste de listes L, et renvoie la concatenation des listes de L dans R.
Acc : Accumulateur, NAcc :nouvel Accumulateur

Exemple:

?- conctListOfList([[a,b,c], [r,b,f], [r,y]], X) ; true.
X = [a, b, c, r, b, f, r, y] ;
true.

?- conctListOfList([[], [[a, b], [a, c], [a, d]], [[b, c], [b, d]], [[c, d]]] , X) ; true.
X = [[a, b], [a, c], [a, d], [b, c], [b, d], [c, d]] ;
true.

*/
conctListOfList(L, R) :- conctListOfList(L, R, []).
conctListOfList([], R, R).
conctListOfList([H | T], R, Acc) :- union(Acc, H, NAcc), conctListOfList(T, R, NAcc).


/*
same(+L, -L2).
Ce predicat prend en parametre une liste L et renvoie dans la liste L2 la meme liste L

Exemple;

?- same([a,b,c,d], L).
L = [a, b, c, d].

*/

same([], []).
same([X|L], [X|L2]) :- same(L, L2).

/*
recupSommetDegre(+SommetAvecEtiq, +ListSommet, -L).
Ce predicat prend en parametre une liste de sommets du graphe avec des etiquettes de la forme [sommet(a, [rock, folk, jazz]), sommet(b, [rock, folk, blues, jazz])]
ainsi qu une liste de sommets de la forme [b, c, d, e] et renvoie dans L une liste de sommets(avec etiquetes) qui appartient a la liste ListSommet donnee en parametre

Exemple:

?- recupSommetDegre([sommet(a, [w, q]), sommet(b, [g, h]), sommet(g, [w, z])], [a,d,g], L).
L = [sommet(a, [w, q]), sommet(g, [w, z])] ;

*/

recupSommetDegre([], _, []).
recupSommetDegre([sommet(X,L)|T], L2, [sommet(X,L)|L3]):- member(X, L2), recupSommetDegre(T, L2 ,L3).
recupSommetDegre([sommet(X,_)|T], L2, L3):- \+member(X, L2), recupSommetDegre(T, L2 ,L3).


/*
 abstractionDegre(+NomF, +Motif, +N, -ListeSOmmet, -ListeArete, -Sommet, -Arete)

Ce predicat prend le nom du fichier('mougel_bis.dot') en parametre, le motif ainsi qu'un degre N et renvoie la liste des sommets du graphe de la forme
[a, b, c, d] dont l etiquette contient le motif donne en parametre et le degre de chaque sommet est superieur ou egal a N.
Il renvoie dans ListeArete la liste des aretes du graphe entre ces sommets de la forme [[a, b], [a, c]].
Il renvoie dans Sommet une liste de sommets avec leurs etiquettes de la forme [sommet(a,[rock,folk,jazz])] , et dans Arete la liste des aretes de la forme
[arete(a, b), arete(a, c)].

Exemple:

?- abstractionDegre('mougel_bis.dot', [jazz, rock], 2, ListeSOmmet, ListeArete, Sommet, Arete).

 Le sous graphe induit par les sommets dont l'étiquette contient le motif est:

att_graph([sommet(a,[rock,folk,jazz]),sommet(b,[rock,folk,blues,jazz]),sommet(c,[rock,folk,jazz]),sommet(d,[rock,folk,jazz]),sommet(g,[rock,folk,jazz]),
sommet(p,[rock,folk,pop,jazz])],[arete(a,b),arete(a,c),arete(a,d),arete(b,c),arete(b,d),arete(c,d),arete(c,g)])

 Le sous graphe induit par les sommets dont le degre est superieur ou egal a N :

att_graph([sommet(a,[rock,folk,jazz]),sommet(b,[rock,folk,blues,jazz]),sommet(c,[rock,folk,jazz]),sommet(d,[rock,folk,jazz])],
[arete(a,b),arete(a,c),arete(a,d),arete(b,c),arete(b,d),arete(c,d)])

ListeSOmmet = [a, b, c, d],
ListeArete = [[a, b], [a, c], [a, d], [b, c], [b, d], [c, d]],
Sommet = [sommet(a, [rock, folk, jazz]), sommet(b, [rock, folk, blues, jazz]), sommet(c, [rock, folk, jazz]), sommet(d, [rock, folk, jazz])],
Arete = [arete(a, b), arete(a, c), arete(a, d), arete(b, c), arete(b, d), arete(c, d)] ;
*/

abstractionDegre(NomF, Motif, N, ListeSommetMD, ListeAreteMD , SommetMD, AreteMD):- sousGrapheMotif(NomF, Motif, _, ListeAreteM, SommetM, _),
                              areteInverse(ListeAreteM, AI),
                              append(AI,ListeAreteM, AP),
                              listeFirstElement(AP, LFE, _),
                              uncode(LFE, UCD),
                              sommetDegre(UCD, N, SD),
                              lesSommetsD(SD,SM),
                              recupSommetDegre(SommetM, SM, SommetMD),
                              aretesDesSommets(SM, ListeAreteM, SM, ADS),
                              conctListOfList(ADS, L),
                              transformerArete(L, AreteMD),
                              conctListOfList(L,ListeSommetMD),
                              same(L, ListeAreteMD),
                              write('\n \n Le sous graphe induit par les sommets dont le degre est superieur ou egal a N : \n\n'),
                              writeln(att_graph(SommetMD,AreteMD)),
                              write('\n \n').
