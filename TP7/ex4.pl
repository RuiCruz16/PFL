% Alínea A

% Factorial (+N, -F) --> Signfica que deveremos considerar o N como input (ou seja, N deve ter sempre um valor para se utilizar) e F será o output da função

factorial(0,1).

factorial(N,F) :- N > 0,
                  N1 is N - 1,
                  factorial(N1, F1),
                  F is N * F1.

% Alínea B

sum_rec(1,1).

sum_rec(N, Sum) :- N > 0,
                   N1 is N - 1,
                   sum_rec(N1, Sum1),
                   Sum is N + Sum1.

% Alínea C

pow_rec(_, 0, 1). 

pow_rec(X, Y, P) :- Y > 0,
                    Y1 is Y - 1,
                    pow_rec(X, Y1, P1),
                    P is X * P1.

% Alínea D

square_rec(0, 0).

square_rec(N, S) :- N > 0,
                    N1 is N -1,
                    square_rec(N1, S1),
                    S is S1 + N + N1.

% Alínea E

fibonacci(0, 0).
fibonacci(1, 1).

% Recursive case: F(N) = F(N-1) + F(N-2)
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% Alínea F

collatz(1, 0).

% N par, N -> N/2.
collatz(N, S) :-
    N > 1,
    0 is N rem 2,
    N1 is N // 2, % tanto `//` como `div` são divisões inteiras, porém `//` é o default, `div` é rounded down
    collatz(N1, S1),
    S is S1 + 1.

% N ímpar, N -> 3N+1.
collatz(N, S) :-
    N > 1,
    1 is N rem 2,
    N1 is 3 * N + 1,
    collatz(N1, S1),
    S is S1 + 1.

% Alínea G

is_prime(X) :-
    X > 1,
    not_has_divisor(X, 2).

not_has_divisor(X, D) :-
    D * D > X.

not_has_divisor(X, D) :-
    D * D =< X,
    X rem D =\= 0, % =\= falha se as duas condições forem iguais, ou seja, se houver um caso de resto 0 que não o próprio número ou 1, retorna false
    D1 is D + 1,
    not_has_divisor(X, D1).
