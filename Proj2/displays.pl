% Displays the welcome message of the game
display_name :- 
    write('----------------------------------'), nl,
    write('------Welcome to Blackstone!------'), nl,
    write('----------------------------------'), nl.

% Displays the current game state
display_game([Board, Player]) :-
    nl, format("Current Player: ~w~n", [Player]), nl,
    display_board(Board, 0).

% Initial board
board([
    [empty, red, empty, red, empty, red, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, red, empty, red, empty, red, empty]
]).

% Displays the current board
display_board(Board, Index) :-
    Board = [FirstRow|_],
    length(Board, NumRows),
    length(FirstRow, NumCols),
    print_column_numbers(NumCols),
    print_horizontal_line(NumCols),
    display_rows(Board, Index, NumRows),
    print_horizontal_line(NumCols),
    print_column_numbers(NumCols).

print_column_numbers(Size) :-
    write('    '),
    print_numbers(1, Size).

print_numbers(Current, Max) :- 
    Current > Max, nl.
print_numbers(Current, Max) :-
    Current =< Max,
    write(' '), write(Current), write(' '),
    NextNum is Current + 1,
    print_numbers(NextNum, Max).

print_horizontal_line(Size) :-
    write('   +'),
    print_dashes(Size),
    write('+'), nl.

print_dashes(0).
print_dashes(Size) :-
    Size > 0,
    write('---'),
    NewSize is Size - 1,
    print_dashes(NewSize).

display_rows([], _, _).
display_rows([Row|Rest], RowIndex, NumRows) :-
    ReverseRowIndex is NumRows - RowIndex,
    ReverseRowIndex > 0,
    write(' '), write(ReverseRowIndex), write(' |'),
    display_row(Row),
    write('| '), write(ReverseRowIndex), nl,
    display_rows(Rest, RowIndex + 1, NumRows).

display_row([]).
display_row([Piece|Rest]) :-
    display_piece(Piece),
    display_row(Rest).

display_piece(empty) :- write(' . ').
display_piece(red)   :- write(' R ').
display_piece(blue)  :- write(' B ').
display_piece(black) :- write(' X ').
