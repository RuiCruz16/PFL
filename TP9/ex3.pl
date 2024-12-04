immature(X):- adult(X), !, fail. % Red Cut --> Sem o Cut, em caso do adult(x) falhar o mesmo entraria em immature(_X).; com o Cut em caso de adult(X) falhar, a função também irá falhar.
immature(_X).

adult(X):- person(X), !, age(X, N), N >=18. % Green Cut --> Se houvesse outro person a seguir, este Cut seria Red, pq haveria outro person para explorar (apenas este seria Red), em caso de terem três persons, apenas o último seria green, por aí em diante.

adult(X):- turtle(X), !, age(X, N), N >=50. % Green Cut

adult(X):- spider(X), !, age(X, N), N>=1. % Green Cut

adult(X):- bat(X), !, age(X, N), N >=5. % Green Cut