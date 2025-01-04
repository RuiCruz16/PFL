% read_input_number(-X, +Acc)
% Function to read a number from the input
read_input_number(X) :- read_input_number(X,0).

read_input_number(Acc, Acc) :-
    peek_code(10), !, get_code(_).

read_input_number(X,Acc) :-
    get_code(Code),
    NumAux is Code - 48,
    NewAcc is Acc * 10 + NumAux,
    read_input_number(X, NewAcc).

% aux_between(+Low, +High, -Value)
aux_between(Low, High, Low) :- Low =< High.
aux_between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    aux_between(Next, High, Value).

% nthX(+List, +Index, -Value)
nthX([Head|_], 0, Head).
nthX([_|Tail], Index, Value) :-
    Index > 0,
    NextIndex is Index - 1,
    nthX(Tail, NextIndex, Value).

% aux_sumlist(+List, -Sum)
aux_sumlist([], 0).
aux_sumlist([Head | Tail], Sum) :-
    aux_sumlist(Tail, PartialSum),
    Sum is Head + PartialSum.

% quicksort(+List, -Sorted)
quicksort([], []).
quicksort([Head|Tail], Sorted) :-
    partition(Head, Tail, Left, Right),
    quicksort(Left, SortedLeft),
    quicksort(Right, SortedRight),
    append(SortedLeft, [Head|SortedRight], Sorted).

% partition(+Pivot, +List, -Left, -Right)
partition(_, [], [], []).
partition([PivotA, PivotB, PivotValue], [[ElemA, ElemB, ElemValue]|Tail], [[ElemA, ElemB, ElemValue]|Left], Right) :-
    ElemValue =< PivotValue,
    partition([PivotA, PivotB, PivotValue], Tail, Left, Right).
partition([PivotA, PivotB, PivotValue], [[ElemA, ElemB, ElemValue]|Tail], Left, [[ElemA, ElemB, ElemValue]|Right]) :-
    ElemValue > PivotValue,
    partition([PivotA, PivotB, PivotValue], Tail, Left, Right).
