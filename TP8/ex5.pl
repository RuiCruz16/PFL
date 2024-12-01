:- use_module(library(lists)).

% Alínea A)

list_to(N, List) :-
    N > 0,
    list_to_helper(1, N, List).

list_to_helper(I, N, []) :-
    I > N.

list_to_helper(I, N, [I | Tail]) :-
    I =< N,
    I1 is I + 1,
    list_to_helper(I1, N, Tail).

% Alínea F)

fibs(N, List) :-
    generate_indices(0, N, Indices),  % Gera os índices de 0 até N
    maplist(fib, Indices, List).       % Aplica a função fib para calcular os Fibonacci para cada índice

% Função auxiliar para gerar os índices de 0 até N
generate_indices(I, N, [I]) :- I =< N.
generate_indices(I, N, [I | Rest]) :- 
    I < N,
    I1 is I + 1,
    generate_indices(I1, N, Rest).

% Função auxiliar para calcular Fibonacci
fib(0, 0).                          % F(0) é 0
fib(1, 1).                          % F(1) é 1
fib(N, F) :-                        
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.