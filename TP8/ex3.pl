% Alínea G)

replicate(0, _, []).

replicate(Amount, Elem, [Elem|Tail]) :-
    Amount > 0,
    NewAmount is Amount - 1,
    replicate(NewAmount, Elem, Tail).

% Alínea I)

insert_elem(0, List1, Elem, [Elem | List1]). % Em caso de ser index 0, o elemento é inserido no início da lista

insert_elem(Index, [H|Tail], Elem, [H | Rest]) :- % Adiciona inicialmente o elemento H à lista, e depois chama a função recursivamente para adicionar o resto dos elementos
    Index > 0,
    NewIndex is Index - 1,
    insert_elem(NewIndex, Tail, Elem, Rest).

% Alínea J)

delete_elem(0, [H|Tail], Elem, Tail). % Em caso de index 0, chegamos ao index desejado, retornar então a lista sem o H (Head)

delete_elem(Index, [H|Tail], Elem, [H|Rest]) :- % Como ainda não chegamos ao index desejado, adicionamos H à lista final e recursivamente calculamos a Tail (restantes elementos) da mesma
    Index > 0,
    NewIndex is Index - 1,
    delete_elem(NewIndex, Tail, Elem, Rest).

% <----------------------------------------------------------->

% Insert element at Index
modify_elem(0, List1, Elem, insert, [Elem | List1]).

% Insert element at Index (recursive case)
modify_elem(Index, [H|Tail], Elem, insert, [H|Rest]) :-
    Index > 0,
    NewIndex is Index - 1,
    modify_elem(NewIndex, Tail, Elem, insert, Rest).

% Delete element at Index
modify_elem(0, [H|Tail], H, delete, Tail).

% Delete element at Index (recursive case)
modify_elem(Index, [H|Tail], Elem, delete, [H|Rest]) :-
    Index > 0,
    NewIndex is Index - 1,
    modify_elem(NewIndex, Tail, Elem, delete, Rest).
