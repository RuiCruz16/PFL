% Alínea A)

list_size([], 0).

list_size([_|Tail], Size) :-
    list_size(Tail, TailSize),
    Size is TailSize + 1.

% Alínea B)

list_sum([],0).

list_sum([H|Tail], Sum) :-
    list_sum(Tail, TailSum),
    Sum is TailSum + H.

% Alínea C)

list_prod([], 1).

list_prod([H|Tail], Prod) :-
    list_prod(Tail, ProdAux),
    Prod is H * ProdAux.

% Alínea D)

inner_product([], [], 0).

inner_product([H1|Tail1], [H2|Tail2], Prod) :-
    inner_product(Tail1, Tail2, ProdAux),
    Prod is ProdAux + H1 * H2.

% Alínea E)

count(_, [], 0).

count(Elem, [H|Tail], Count) :-
    Elem == H,
    count(Elem, Tail, CountAux),
    Count is CountAux + 1.

count(Elem, [H|Tail], Count) :-
    Elem \= H,
    count(Elem, Tail, Count).
