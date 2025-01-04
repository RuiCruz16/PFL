% display_name/0
% Displays the welcome message of the game
display_name :- 
    write('----------------------------------'), nl,
    write('------Welcome to Blackstone!------'), nl,
    write('----------------------------------'), nl.

% display_game(+GameState)
% Displays the current game state
display_game([Board, Player]) :-
    nl, format("Current Player: ~w~n", [Player]), nl,
    display_board(Board, 0).

% board(+Size, -Board)
% Returns the initial board for the given size
board(6, [
    [empty, red, empty, red, empty, empty],
    [empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty],
    [empty, empty, red, empty, red, empty]
]).

board(8, [
    [empty, red, empty, red, empty, red, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, red, empty, red, empty, red, empty]
]).

board(10, [
    [empty, red, empty, red, empty, red, empty, red, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, empty, blue],
    [blue, empty, empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, red, empty, red, empty, red, empty, red, empty]
]).

% display_board(+Board, +Index)
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

% print_column_numbers(+Size)
% Prints the column numbers
print_column_numbers(Size) :-
    write('    '),
    print_numbers(1, Size).

% print_numbers(+Current, +Max)
% Prints the numbers from Current to Max (size of the board)
print_numbers(Current, Max) :- 
    Current > Max, nl.
print_numbers(Current, Max) :-
    Current =< Max,
    format(' ~w ', [Current]),
    NextNum is Current + 1,
    print_numbers(NextNum, Max).

% print_horizontal_line(+Size)
% Prints a horizontal line
print_horizontal_line(Size) :-
    write('   +'),
    print_dashes(Size),
    write('+'), nl.

% print_dashes(+Size)
% Prints dashes
print_dashes(0).
print_dashes(Size) :-
    Size > 0,
    write('---'),
    NewSize is Size - 1,
    print_dashes(NewSize).

% display_rows(+Board, +RowIndex, +NumRows)
% Displays the rows of the board
display_rows([], _, _).
display_rows([Row|Rest], RowIndex, NumRows) :-
    ReverseRowIndex is NumRows - RowIndex,
    ReverseRowIndex > 0,
    format('~|~t~w~2+ |', [ReverseRowIndex]),
    display_row(Row),
    format('| ~|~t~w~2+', [ReverseRowIndex]), nl,
    display_rows(Rest, RowIndex + 1, NumRows).

% display_row(+Row)
% Displays a row of the board
display_row([]).
display_row([Piece|Rest]) :-
    display_piece(Piece),
    display_row(Rest).

% display_piece(+Piece)
% Displays a piece
display_piece(empty) :- write(' . ').
display_piece(red)   :- write(' R ').
display_piece(blue)  :- write(' B ').
display_piece(black) :- write(' X ').
