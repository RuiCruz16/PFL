:-dynamic saga/4, movie/8.
:- use_module(library(lists)).

%saga(SagaID, Saga Name, Number of Movies in Saga, Creator)
saga(1, 'Jurassic Park',  6, 'Michael Crichton').
saga(2, 'Indiana Jones',  4, 'George Lucas').
saga(3, 'Star Wars',      9, 'George Lucas').
saga(4, 'Harry Potter',   0, 'J. K. Rowling').
saga(6, 'Jaws',           0, 'Peter Benchley').

%movie(Movie Title, Year of Release, SagaID, Duration, IMDB Score, Director, Composer, Cast)
movie('Jurassic Park',                  1993, 1, 127, 8.2, 'Steven Spielberg', 'John Williams',     ['Sam Neill', 'Jeff Goldblum', 'Laura Dern', 'BD Wong']).
movie('The Lost World: Jurassic Park',  1997, 1, 129, 6.5, 'Steven Spielberg', 'John Williams',     ['Jeff Goldblum', 'Julianne Moore', 'Vince Vaughn', 'Richard Schiff']).
movie('Jurassic Park III',              2001, 1,  92, 5.9, 'Joe Johnston',     'Don Davis',         ['Sam Neill', 'William H. Macy', 'Téa Leoni']).
movie('Jurassic World',                 2015, 1, 124, 6.9, 'Colin Trevorrow',  'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'Irrfan Khan', 'BD Wong']).
movie('Jurassic World: Fallen Kingdom', 2018, 1, 128, 6.1, 'J.A. Bayona',      'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'James Cromwell', 'BD Wong']).
movie('Jurassic World: Dominion',       2022, 1, 147, 5.6, 'Colin Trevorrow',  'Michael Giacchino', ['Chris Pratt', 'Bryce Dallas Howard', 'Campbell Scott', 'BD Wong']).

movie('Raiders of the Lost Ark',       1981, 2, 115, 8.4, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Karen Allen', 'John Rhys-Davies']).
movie('The Temple of Doom',            1984, 2, 118, 7.5, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Kate Capshaw', 'Ke Huy Quan']).
movie('The Last Crusade',              1989, 2, 127, 8.2, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Alison Doody', 'Sean Connery']).
movie('Kingdom of the Crystal Skull',  2008, 2, 122, 6.2, 'Steven Spielberg', 'John Williams', ['Harrison Ford', 'Karen Allen', 'Shia LaBeouf']).

movie('The Phantom Menace',       1999, 3, 136, 6.5, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Liam Neeson', 'Natalie Portman', 'Ian McDiarmid']).
movie('Attack of the Clones',     2002, 3, 142, 6.6, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Hayden Christensen', 'Natalie Portman', 'Christopher Lee']).
movie('Revenge of the Sith',      2005, 3, 140, 7.6, 'George Lucas',     'John Williams', ['Ewan McGregor', 'Hayden Christensen', 'Natalie Portman', 'Christopher Lee']).
movie('A New Hope',               1977, 3, 121, 8.6, 'George Lucas',     'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Alec Guinness']).
movie('The Empire Strikes Back',  1980, 3, 124, 8.7, 'Irvin Kershner',   'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Billy Dee Williams']).
movie('Return of the Jedi',       1983, 3, 131, 8.3, 'Richard Marquand', 'John Williams', ['Harrison Ford', 'Mark Hamill', 'Carrie Fisher', 'Ian McDiarmid']).
movie('The Force Awakens',        2015, 3, 138, 7.8, 'J. J. Abrams',     'John Williams', ['Daisy Ridley', 'Harrison Ford', 'Mark Hamill', 'Carrie Fisher']).
movie('The Last Jedi',            2017, 3, 152, 6.9, 'Rian Johnson',     'John Williams', ['Daisy Ridley', 'Mark Hamill', 'Carrie Fisher', 'John Boyega']).
movie('The Rise of Skywalker',    2019, 3, 141, 6.4, 'J. J. Abrams',     'John Williams', ['Daisy Ridley', 'Mark Hamill', 'John Boyega', 'Adam Driver']).

same_director(Movie1, Movie2) :-
    movie(Movie1, _, _, _, _, Director, _, _),
    movie(Movie2, _, _, _, _, Director, _, _).

movie_from_saga(Movie, Saga) :-
    movie(Movie, _, SagaId, _, _, _, _, _),
    saga(SagaId, Saga, _, _).

saga_longest_movie(Saga, Movie) :-
    saga(SagaId, Saga, _, _),
    movie(Movie, _, SagaId, Duration1, _, _, _, _),
    \+ (movie(_, _, SagaId, Duration2, _, _, _, _), Duration2 > Duration1).

add_movie_to_saga(Saga, Movie, Year, Duration, Score, Director, Compose, Cast) :-
    saga(SagaId, Saga, Num, Creator),
    \+ (movie(Movie, Year, SagaId, Duration, Score, Director, Compose, Cast)),
    assert(movie(Movie, Year, SagaId, Duration, Score, Director, Compose, Cast)),
    NumAux is Num + 1,
    retract(saga(SagaId, _, _, _)),
    assert(saga(SagaId, Saga, NumAux, Creator)).

movies_from_saga(Saga, Movies) :-
    saga(SagaId, Saga, _, _),
    collect_movies(SagaId, [], MoviesAux),
    keysort(MoviesAux, MoviesTemp),
    get_movies(MoviesTemp, [], MoviesFinal),
    invert_order(MoviesFinal, [], Movies).

collect_movies(SagaId, Acc, Movies) :-
    movie(Movie, Year, SagaId, _, _, _, _, _),
    \+ memberchk(Year-Movie, Acc),
    collect_movies(SagaId, [Year-Movie|Acc], Movies).
collect_movies(_, Movies, Movies).

get_movies([], Movies, Movies).
get_movies([(_-Movie) | T], Acc, Movies) :-
    get_movies(T, [Movie | Acc], Movies).

invert_order([], Movies, Movies).
invert_order([H | T], Acc, Movies) :-
    append([H], Acc, Acc1),
    invert_order(T, Acc1, Movies).

saga_cast(Saga, Actors) :-
    findall(ListofActors, (saga(SagaId, Saga, _, _), movie(_, _, SagaId, _, _, _, _, ListofActors)), ActorsTemps),
    append(ActorsTemps, ActorsFinal),
    sort(ActorsFinal, Actors).

sample_cast(Saga, Cast) :-
    saga_cast(Saga, ActorsList),
    odd_list(ActorsList, 0, [], Cast).

odd_list([], _, Cast, Cast).
odd_list([H | T], Index, Acc, Cast) :-
    0 =:= Index rem 2,
    NewIndex is Index + 1,
    odd_list(T, NewIndex, [H | Acc], Cast).

odd_list([_ | T], Index, Acc, Cast) :-
    1 =:= Index rem 2,
    NewIndex is Index + 1,
    odd_list(T, NewIndex, Acc, Cast).

composer_rating(Composer, AvgScore) :-
    findall(Score, movie(_, _, _, _, Score, _, Composer, _), ComposerList),
    sumlist(ComposerList, Sum),
    length(ComposerList, Aux),
    AvgScore is Sum / Aux.

most_successful_composer(Composer1, Composer2, MostSuccessFul):-
    composer_rating(Composer1, R1),
    composer_rating(Composer2, R2),
    R1 >= R2,
    MostSuccessFul = Composer1.

most_successful_composer(_Composer1, Composer2, MostSuccessful):-
    composer_rating(Composer2, R2),
    composer_rating(_Composer1, R1),
    R2 >= R1,
    MostSuccessful = Composer2.

:-op(500, xfx, composed_for).

Composer composed_for Movie :-
    movie(Movie, _, _, _, _, _, Composer, _).

connected(Person1, Person2):-
    connected2(Person1, Person2),
    Person1 \= Person2.
connected(Person1, Person2):-
    connected2(Person2, Person1),
    Person1 \= Person2.
    
connected2(Person1, Person2):-
    movie(_T, _Y, _S, _D, _Sc, Person1, Person2, _).
connected2(Person1, Person2):-
    movie(_T, _Y, _S, _D, _Sc, Person1, _C, Cast),
    member(Person2, Cast).
connected2(Person1, Person2):-
    movie(_T, _Y, _S, _D, _Sc, _Dir, Person1, Cast),
    member(Person2, Cast).
connected2(Person1, Person2):-
    movie(_T, _Y, _S, _D, _Sc, _Dir, _Comp, Cast),
    member(Person1, Cast),
    member(Person2, Cast).

connected_degree(Person1, Person2, Degree):-
	connected_degree_bfs([ [Person1] ], Person2, Degree).

connected_degree_bfs([ [Person1|R] | _], Person1, Degree):- !,
	length(R, Degree).
connected_degree_bfs([ [Person1|R] | T ], Person2, Degree):-
	setof(Next, ( connected(Person1, Next),
		      \+ (member(Next, [Person1|R])) ), List),
	append_all(List, [Person1|R], ToSee),
	append(T, ToSee, NextList),
	connected_degree_bfs(NextList, Person2, Degree).

append_all([], _List, []).
append_all([H|T], List, [ [H|List] |NT]):-
	append_all(T, List, NT).
