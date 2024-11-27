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

married(Person1, Person2, Year).
divorced(Person1, Person2, Year).

married(jay, gloria, 2008).
married(jay, dede, 1968).
divorced(jay, dede, 2003).

% ---------------------------------------------------------------------------------------------------------

ancestor_of(X,Y) :- parent(X,Y).

ancestor_of(X,Y) :- parent(X,Z), ancestor_of(Z,Y).

descendant_of(X,Y) :- parent(Y,X).

descendant_of(X,Y) :- parent(Y,Z), descendant_of(X,Z).

marriage_years(X, Y, Years) :- married(X, Y, MarriageYear), divorced(X, Y, DivorceYear), Years is DivorceYear - MarriageYear.

% Alínea D)

% i. Who is Gloria’s descendant, but not Jay’s?

% - descendant_of(X, gloria), \+ descendant_of(X, jay). (The `\+` operator in Prolog is used for negation (i.e., to ensure that the second condition does not hold). This query will give you all of Gloria's descendants who are not Jay's descendants)

% ii. What ancestors do Haley and Lily have in common?

% - ancestor_of(Ancestor, haley), ancestor_of(Ancestor, lily).

% iii. To whom were/are both Dede and Gloria married?

% - married(Person, dede, _), married(Person, gloria, _).

born(jay, 1946-5-23).
born(claire, 1970-11-13).
born(mitchell, 1973-7-10).

% Alínea E)

% Receives two dates in the format yyyy-mm-dd
before(X, Y) :- X @< Y. % The `@<` operator in Prolog is used for lexicographical comparison of strings, and it works as expected for dates in the yyyy-mm-dd format. It compares the dates in a "natural" order: earlier dates are "less than" later ones.

% The first rule states that X is older than Y if X's birthdate is before Y's (using the before/2 predicate).
% The second rule states that Y is older than X if Y's birthdate is before X's.
older(X, Y, X) :- born(X, DateX), born(Y, DateY), DateX @< DateY.
older(X, Y, Y) :- born(X, DateX), born(Y, DateY), DateX @> DateY.

% The oldest/1 predicate will succeed if there is no one in the knowledge base whose birthdate is earlier than X's.
% We use negation (\+) to ensure that there is no Y whose birthdate comes before X's.
oldest(X) :- born(X, DateX), \+ (born(Y, DateY), DateY @< DateX).