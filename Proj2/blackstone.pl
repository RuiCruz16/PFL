:- use_module(library(lists)).

play :- 
    display_name,
    display_main_menu.

display_name :- 
    write('----------------------------------'), nl,
    write('------Welcome to Blackstone!------'), nl,
    write('----------------------------------'), nl.

display_main_menu :- 
    write('1. Start Game'), nl,
    write('2. Instructions'), nl,
    write('3. Exit'), nl,
    write('Please select an option: '),
    read_input_number(MenuOption),
    handle_main_menu_input(MenuOption).

read_input_number(X) :- read_input_number(X,0).

read_input_number(Acc, Acc) :-
    peek_code(10), !, get_code(_).

read_input_number(X,Acc) :-
    get_code(Code),
    NumAux is Code - 48,
    NewAcc is Acc * 10 + NumAux,
    read_input_number(X, NewAcc).

handle_main_menu_input(1) :- 
    nl, display_game_menu.

handle_main_menu_input(2) :-
    nl, display_instructions.

handle_main_menu_input(3) :-
    nl, write('Exiting game...').

handle_main_menu_input(_) :-
    nl, write('Invalid option, try again.'), nl,
    display_main_menu.

display_instructions :-
    write('Blackstone Instructions:'), nl,
    write('1. Blackstone is a two-player strategy game played on a square board of any even size.'), nl,
    write('2. The board should be at least 6x6.'), nl,
    write('3. The board starts with red and blue stones on the corners.'), nl,
    write('4. Players take turns moving their stones like chess queens (any direction).'), nl,
    write('5. After moving, a neutral black stone is placed on the square vacated.'), nl,
    write('6. If a stone becomes blocked (no valid moves), it is removed from the board.'), nl,
    write('7. The objective is to block and remove all opponent stones while keeping your own.'), nl,
    write('8. If a move results in all red or blue stones being removed, the other player wins.'), nl,
    nl,
    write('Press any key to return to the main menu.'), nl,
    skip_line,
    display_main_menu.

display_game_menu :-
    write('Select the mode you want to play:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Return to main menu'), nl,
    write('Please select an option: '),
    read_input_number(GameOption),
    handle_game_menu_input(GameOption).

% TODO: Implementar modo de jogo entre jogadores
handle_game_menu_input(1) :-
    nl, write('Starting Player vs Player game...'), nl,
    get_game_state(GameState),
    game_loop(GameState).

% TODO: Implementar modo de jogo contra o computador
handle_game_menu_input(2) :-
    nl, write('Coming soon...').

% TODO: Implementar modo de jogo entre computadores
handle_game_menu_input(3) :-
    nl, write('Coming soon...').

handle_game_menu_input(4) :-
    nl, write('Returning to main menu...'), nl,
    display_main_menu.

handle_game_menu_input(_) :-
    nl, write('Invalid option, try again.'), nl,
    display_game_menu.

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

player(red, player1).
player(blue, player2).

% Ig que possa ser o initial_state
get_game_state([Board, red]) :-
    board(Board).

game_loop([Board, Player]) :-
    display_game([Board, Player]),
    nl, choose_piece([Board, Player], PieceCoords),
    % format("Selected piece coordinates: ~w~n", [PieceCoords]).
    valid_moves([Board, Player], PieceCoords, ListOfMoves),
    format("Valid moves for selected piece: ~w~n", [ListOfMoves]).

display_game([Board, Player]) :-
    nl, format("Current Player: ~w~n", [Player]), nl,
    display_board(Board, 0).

switch_player(red, blue).
switch_player(blue, red).

choose_piece([Board, Player], Coords) :-
    write('Select a piece to move'), nl,
    write('Enter X coordinate: '),
    read_input_number(X),
    write('Enter Y coordinate: '),
    read_input_number(Y),
    length(Board, BoardSize),
    validate_coordinates(X, Y, BoardSize, [Board, Player], Coords).

validate_coordinates(X, Y, BoardSize, [Board, Player], Coords) :-
    (X > 0, X =< BoardSize, Y > 0, Y =< BoardSize),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    nthX(Board, RowIndex, Row),
    nthX(Row, ColIndex, Piece),
    validate_piece(RowIndex, ColIndex, Piece, Player, BoardSize, [Board, Player], Coords).

% FIXME: singleton variables no X, Y e BoardSize
validate_coordinates(X, Y, BoardSize, [Board, Player], Coords) :-
    write('Coordinates out of bounds.'), nl, 
    nl, choose_piece([Board, Player], Coords).

validate_piece(_, _, empty, _, _, [Board, Player], Coords) :-
    write('No piece at the selected coordinates. Please try again.'), nl,
    nl, choose_piece([Board, Player], Coords).

validate_piece(_, _, black, _, _, [Board, Player], Coords) :-
    write('Cannot move a black piece. Please try again.'), nl,
    nl, choose_piece([Board, Player], Coords).

validate_piece(_, _, Piece, Player, _, [Board, Player], Coords) :-
    Piece \= Player,
    write('This is not your piece. Please try again.'), nl,
    nl, choose_piece([Board, Player], Coords).

validate_piece(RowIndex, ColIndex, Player, Player, BoardSize, _, (X, Y)) :-
    X is ColIndex + 1,
    Y is BoardSize - RowIndex.

valid_moves([Board, Player], (X, Y), ListOfMoves) :-
    length(Board, BoardSize),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    findall(
        (NX, NY),
        (valid_move([Board, Player], RowIndex, ColIndex, NX, NY)),
        ListOfMoves
    ).

% FIXME: singleton variable no Player
valid_move([Board, Player], RowIndex, ColIndex, NX, NY) :-
    length(Board, BoardSize),
    direction(DX, DY),
    generate_moves(RowIndex, ColIndex, DX, DY, Board, BoardSize, NX, NY).

% All directions
direction(-1, 0).
direction(1, 0).
direction(0, -1).
direction(0, 1).
direction(-1, -1).
direction(-1, 1).
direction(1, -1).
direction(1, 1).

% TODO: Entender esta função, como ela funciona e como ela é chamada
generate_moves(Row, Col, DX, DY, Board, BoardSize, NX, NY) :-
    NRow is Row + DX,
    NCol is Col + DY,
    within_bounds(NRow, NCol, BoardSize),
    nthX(Board, NRow, RowList),
    nthX(RowList, NCol, Cell),
    Cell = empty,
    NX is NCol + 1,
    NY is BoardSize - NRow.

generate_moves(Row, Col, DX, DY, Board, BoardSize, NX, NY) :-
    NRow is Row + DX,
    NCol is Col + DY,
    within_bounds(NRow, NCol, BoardSize),
    nthX(Board, NRow, RowList),
    nthX(RowList, NCol, Cell),
    Cell = empty,
    generate_moves(NRow, NCol, DX, DY, Board, BoardSize, NX, NY).

within_bounds(Row, Col, BoardSize) :-
    Row >= 0, Row < BoardSize,
    Col >= 0, Col < BoardSize.

nthX([Head|_], 0, Head).
nthX([_|Tail], Index, Value) :-
    Index > 0,
    NextIndex is Index - 1,
    nthX(Tail, NextIndex, Value).

display_board(Board, Index) :-
    Board = [FirstRow|_],
    length(Board, NumRows),     % Get number of rows
    length(FirstRow, NumCols),  % Get number of columns
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
    (ReverseRowIndex > 0 ->
        write(' '), write(ReverseRowIndex), write(' |'),
        display_row(Row),
        write('| '), write(ReverseRowIndex), nl,
        display_rows(Rest, RowIndex + 1, NumRows)
    ;   true).

display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

display_cell(empty) :- write(' . ').
display_cell(red)   :- write(' R ').
display_cell(blue)  :- write(' B ').
display_cell(black) :- write(' X ').
