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

% --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

% Alínea A)

:- dynamic male/1, female/1.

add_person(Gender, Name) :-
    \+ male(Name),
    \+ female(Name),
    (Gender = male -> assertz(male(Name));
     Gender = female -> assertz(female(Name))).

% Alínea C)

:- dynamic male/1, female/1, parent/2.

remove_person :-
    write('Enter the name of the person to remove: '), nl,
    read(Name),
    remove_person(Name).

remove_person(Name) :-
    (male(Name) ; female(Name)), 
    retractall(male(Name)),
    retractall(female(Name)), % preserves definition of female/male (the `abolish` predicate would remove the definition)
    retractall(parent(Name, _)),
    retractall(parent(_, Name)),
    write(Name), write(' and their relationships have been removed.'), nl.

remove_person(Name) :-
    \+ male(Name), \+ female(Name),
    write(Name), write(' does not exist in the knowledge base.'), nl.

% Alínea F) --> Meta-Predicates

:- dynamic male/1, female/1.

family(F) :-
    var(F), !,
    findall(Name, (male(Name) ; female(Name)), F).

family(F) :-
    is_list(F), !,
    all_family_members(Members),
    subset(F, Members).

family(F) :-
    atom(F), !,
    (male(F) ; female(F)).

all_family_members(Members) :-
    findall(Name, (male(Name) ; female(Name)), Members).

% Alínea G)

:- dynamic parent/2.

print_descendents(Person) :-
    write('Children:'), nl,
    print_children(Person),
    write('Grandchildren:'), nl,
    print_grandchildren(Person).

print_children(Person) :-
    (parent(Person, Child), 
     write(' '), write(Child), nl,
     fail; true).

print_grandchildren(Person) :-
    (parent(Person, Child), 
     parent(Child, Grandchild), 
     write(' '), write(Grandchild), nl,
     fail; true).
