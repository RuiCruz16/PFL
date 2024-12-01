% Alínea A)

is_ordered([]).
is_ordered([_]). 

is_ordered([X, Y | Tail]) :-
    X =< Y,
    is_ordered([Y | Tail]). 

% Alínea B)

insert_ordered(V, [], [V]).

insert_ordered(V, [H|Tail1], [H|Tail2]) :-
    H < V,
    insert_ordered(V, Tail1, Tail2).

insert_ordered(V, [H|Tail1], [V,H|Tail1]) :-
    V =< H.

% Alínea C)

insert_sort([], []).

insert_sort([H|T], OrderedList) :-
    insert_sort(T, SortedTail),
    insert_ordered(H, SortedTail, OrderedList).
