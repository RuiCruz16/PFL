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
% Auxiliary function to check if Value is between Low and High
aux_between(Low, High, Low) :- Low =< High.
aux_between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    aux_between(Next, High, Value).

% aux_sumlist(+List, -Sum)
% Auxiliary function to calculate the sum of the elements of a list
aux_sumlist([], 0).
aux_sumlist([Head | Tail], Sum) :-
    aux_sumlist(Tail, PartialSum),
    Sum is Head + PartialSum.

% quicksort(+List, -Sorted)
% Sorts the list using the quicksort algorithm
quicksort([], []).
quicksort([Head|Tail], Sorted) :-
    partition(Head, Tail, Left, Right),
    quicksort(Left, SortedLeft),
    quicksort(Right, SortedRight),
    append(SortedLeft, [Head|SortedRight], Sorted).

% partition(+Pivot, +List, -Left, -Right)
% Partitions the list into two lists, one with elements smaller than the pivot and the other with elements greater than the pivot
partition(_, [], [], []).
partition([PivotA, PivotB, PivotValue], [[ElemA, ElemB, ElemValue]|Tail], [[ElemA, ElemB, ElemValue]|Left], Right) :-
    ElemValue =< PivotValue,
    partition([PivotA, PivotB, PivotValue], Tail, Left, Right).
partition([PivotA, PivotB, PivotValue], [[ElemA, ElemB, ElemValue]|Tail], Left, [[ElemA, ElemB, ElemValue]|Right]) :-
    ElemValue > PivotValue,
    partition([PivotA, PivotB, PivotValue], Tail, Left, Right).
