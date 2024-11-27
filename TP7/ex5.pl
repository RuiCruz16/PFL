factorial(N,F) :- factorial(N,F,1).

factorial(0,F,F).

factorial(N,F, Acc) :- N > 0,
                  N1 is N - 1,
                  Acc1 is Acc * N,
                  factorial(N1, F, Acc1).


sum_rec(N, Sum) :- sum_rec(N, Sum, 1).

sum_rec(1, Sum, Sum).

sum_rec(N, Sum, Acc) :- N > 0,
                   N1 is N - 1,
                   Acc1 is Acc + N,
                   sum_rec(N1, Sum, Acc1).


pow_rec(X, Y, P) :- pow_rec(X, Y, P, 1).

pow_rec(_, 0, P, P).

pow_rec(X, Y, P, Acc) :- Y > 0,
                    Y1 is Y - 1,
                    Acc1 is Acc * X,
                    pow_rec(X, Y1, P, Acc1).


square_rec(N, S) :- square_rec(N, S, 0).

square_rec(0, S, S).

square_rec(N, S, Acc) :- N > 0,
                         N1 is N - 1,
                         Acc1 is Acc + N + N1,
                         square_rec(N1, S, Acc1).


fibonacci(N, F) :- fibonacci(N, F, 0, 1).

fibonacci(0, F1, F1, _).

fibonacci(N, F, F1, F2) :- N > 0,
                           N1 is N - 1,
                           F3 is F1 + F2,
                           fibonacci(N1, F, F2, F3). % 0 - 1 / 1 - 1 / 1 - 2 / 2 - 3 / ... 

% Na alínea b) é suposto usar o trace mode para comparar as calls feitas na regular recursion e na tail recursion, verificando que a tail recursion é a mais eficaz das duas
% b) Regular Recursion - Time Complexity: O(2^N) (Recomputes Fibonacci numbers multiple times, resulting in exponential growth) / Tail Recursion - Time Complexity: O(N) (Each Fibonacci number is calculated exactly once, carried forward via accumulators)
