% Alínea A)

print_full_list([]) :- !.

print_full_list([H]) :-
    write(H), !. 

print_full_list([H|Tail]) :-
    write(H),
    write(', '),
    print_full_list(Tail).

% Alínea C)

print_matrix([]) :- !.

print_matrix([H | Tail]) :-
    write('['),
    print_full_list(H),
    write(']'), nl, 
    print_matrix(Tail).

% Alínea D)

print_numbered_matrix(List) :- print_numbered_matrix_helper(List, 1).

print_numbered_matrix_helper([]) :- !.

print_numbered_matrix_helper([H|Tail], Acc) :-
    write(Acc),
    write(' '),
    write('['),
    print_full_list(H),
    write(']'), nl, 
    Acc1 is Acc + 1,
    print_numbered_matrix_helper(Tail, Acc1).

% Alínea E)

print_list([], Start, _, End) :-
    write(Start),
    write(End), !, nl.

print_list([H|Tail], Start, Sep, End) :-
    write(Start),
    write(H),
    print_list_tail(Tail, Sep, End).

print_list_tail([], _, End) :-
    write(End), !, nl.

print_list_tail([H|Tail], Sep, End) :-
    write(Sep),
    write(H),
    print_list_tail(Tail, Sep, End).