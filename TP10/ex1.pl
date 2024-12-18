% Gender facts
male(frank).
male(jay).
male(javier).
male(phil).
male(mitchell).
male(joe).
male(manny).
male(cameron).
male(pameron).
male(bo).
male(dylan).
male(alex).
male(luke).
male(rexford).
male(george).
male(calhoun).
female(grace).
female(dede).
female(gloria).
female(barb).
female(merle).
female(claire).
female(haley).
female(lily).
female(poppy).

% Parent facts

parent(frank, phil).
parent(grace, phil).
parent(dede, claire).
parent(dede, mitchell).
parent(jay, claire).
parent(jay, mitchell).
parent(jay, joe).
parent(gloria, joe).
parent(gloria, manny).
parent(javier, manny).
parent(barb, cameron).
parent(barb, pameron).
parent(merle, cameron).
parent(merle, pameron).
parent(pameron, calhoun).
parent(bo, calhoun).
parent(phil, haley).
parent(phil, alex).
parent(phil, luke).
parent(claire, haley).
parent(claire, alex).
parent(claire, luke).
parent(dylan, george).
parent(dylan, poppy).
parent(haley, george).
parent(haley, poppy).
parent(mitchell, lily).
parent(mitchell, rexford).
parent(cameron, lily).
parent(cameron, rexford).

grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), parent(W, X), parent(W, Y), Z \= W, X \= Y. 

father(X, Y) :- male(X), parent(X, Y).

grandmother(X) :- female(X), grandparent(X,Y).

halfsibling(X, Y) :- parent(Z, X), parent(Z, Y), parent(A, X), parent(B, Y), A \= B, Z \= A, Z \= B, X \= Y.  

cousin(X,Y) :- parent(A,X), parent(B,Y), sibling(A,B), A \= B, X \= Y.

uncle(X,Y) :- parent(Z,Y), sibling(X,Z), Z \= X.

% i - Yes
% ii - No
% iii - Yes
% iv - dede, jay
% v - joe, manny
% vi - haley, alex, luke, lily, rexford
% vii - dede, jay, barb, merle
% viii - no
% ix - haley, alex

% are Haley and Lily cousins? True
% who is Luke’s father? Phil
% who is Lily’s uncle? Claire, Joe, Pameron
% who is a grandmother? Grace, Dede, Barb, Merle, Claire
% list all pairs of siblings (Claire, Mitchell), (Cameron, Pameron), (Haley, Alex), (Haley, Luke), (Alex, Luke), (George, Poppy), (Lily, Rexford)
% all half-siblings (Claire, Joe), (Mitchell, Joe), (Joe, Manny)

married(Person1, Person2, Year).
divorced(Person1, Person2, Year).

married(jay, gloria, 2008).
married(jay, dede, 1968).
divorced(jay, dede, 2003).

% -------------------------------------------------------------------

% Alínea A)

children(Person, Children) :- findall(Child, parent(Person, Child), Children). 

% Alínea C)

family(F) :- findall(Person, (parent(Person, _); parent(_, Person)), F). % `;` representa `ou`

% Alínea D)

couple(Person1-Person2) :-
    findall(Person1-Person2, 
            (parent(Person2, Child), parent(Person1, Child), Person1 \= Person2, Person1 @< Person2), 
            Couples),
    member(Person1-Person2, Couples).

% Alínea E)

couples(List) :-
    setof(Person1-Person2, 
          Child^(parent(Person1, Child), % Child^ significa que não queremos agrupar por Child
                 parent(Person2, Child), 
                 Person1 \= Person2, 
                 Person1 @< Person2), 
          List).
