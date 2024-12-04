% Alínea A)

print_n(0,S) :- !.

print_n(N, S) :- 
    N > 0,
    N1 is N - 1,
    write(S),
    print_n(N1,S). 

% Alínea B)

print_text(Text, Symbol, Padding) :-
    write(Symbol),
    print_padding(Padding),
    write(Text),
    print_padding(Padding),
    write(Symbol), !.

print_padding(0).
print_padding(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    print_padding(N1).

% Alínea D)

read_number(X) :- read_number(X,0).

read_number(Acc, Acc) :-
    peek_code(10), !, get_code(_).

read_number(X,Acc) :-
    get_code(Code),
    NumAux is Code - 48,
    NewAcc is Acc * 10 + NumAux,
    read_number(X, NewAcc).

% Alínea E)

read_until_between(Min, Max, Value) :-
    read_number(Value),
    Value >= Min,
    Value =< Max, !.

read_until_between(Min, Max, Value) :-
    read_until_between(Min, Max, Value).