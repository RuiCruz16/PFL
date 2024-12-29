player(red, player1).
player(blue, player2).

switch_player(red, blue).
switch_player(blue, red).

initial_state([Board, red]) :-
    board(Board).

game_loop([Board, Player]) :-
    game_over([Board, Player], draw),
    nl, write('Game over! It\'s a draw!'), nl.

game_loop([Board, Player]) :-
    game_over([Board, Player], Winner),
    nl, format('Game over! The winner is ~w!~n', [Winner]).

game_loop([Board, Player]) :-
    \+ game_over([Board, Player], _),
    display_game([Board, Player]),
    nl, choose_piece([Board, Player], PieceCoords),
    % format("Selected piece coordinates: ~w~n", [PieceCoords]).
    valid_moves([Board, Player], PieceCoords, ListOfMoves),
    format("Valid moves for selected piece: ~w~n", [ListOfMoves]),
    nl, choose_new_position(ListOfMoves, NewCoords),
    %format("New position coordinates: ~w~n", [NewCoords]),
    move([Board, Player], PieceCoords, NewCoords, [NewBoard, NewPlayer]),
    check_blocked_pieces(NewBoard, FinalBoard),
    game_loop([FinalBoard, NewPlayer]).

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

validate_coordinates(_, _, _, [Board, Player], Coords) :-
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

valid_move([Board, _], RowIndex, ColIndex, NX, NY) :-
    length(Board, BoardSize),
    direction(DX, DY),
    generate_moves(RowIndex, ColIndex, DX, DY, Board, BoardSize, NX, NY).

% All directions that a piece can move
direction(-1, 0).
direction(1, 0).
direction(0, -1).
direction(0, 1).
direction(-1, -1).
direction(-1, 1).
direction(1, -1).
direction(1, 1).

% FIXME: ESTA GENERATE_MOVES ESTÁ SUS
generate_moves(Row, Col, DX, DY, Board, BoardSize, NX, NY) :-
    NRow is Row + DX,
    NCol is Col + DY,
    within_bounds(NRow, NCol, BoardSize),
    nthX(Board, NRow, RowList),
    nthX(RowList, NCol, Piece),
    Piece = empty,
    NX is NCol + 1,
    NY is BoardSize - NRow.

generate_moves(Row, Col, DX, DY, Board, BoardSize, NX, NY) :-
    NRow is Row + DX,
    NCol is Col + DY,
    within_bounds(NRow, NCol, BoardSize),
    nthX(Board, NRow, RowList),
    nthX(RowList, NCol, Piece),
    Piece = empty,
    generate_moves(NRow, NCol, DX, DY, Board, BoardSize, NX, NY).

within_bounds(Row, Col, BoardSize) :-
    Row >= 0, Row < BoardSize,
    Col >= 0, Col < BoardSize.

choose_new_position(ListOfMoves, NewCoords) :-
    write('Select a new position to move the piece'), nl,
    write('Enter X coordinate: '),
    read_input_number(X),
    write('Enter Y coordinate: '),
    read_input_number(Y),
    validate_new_position(X, Y, ListOfMoves, NewCoords).

validate_new_position(X, Y, ListOfMoves, (X, Y)) :-
  member((X, Y), ListOfMoves).

validate_new_position(X, Y, ListOfMoves, NewCoords) :-
    \+ member((X, Y), ListOfMoves),
    write('Invalid move. The selected coordinates are not in the list of valid moves. Please try again.'), nl,
    nl, choose_new_position(ListOfMoves, NewCoords).

move([Board, Player], (X, Y), (NX, NY), [NewBoard, NewPlayer]) :-
    length(Board, BoardSize),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    NRowIndex is BoardSize - NY,
    NColIndex is NX - 1,
    nthX(Board, RowIndex, Row),
    nthX(Row, ColIndex, Piece),
    replace(Board, RowIndex, ColIndex, black, TempBoard),
    replace(TempBoard, NRowIndex, NColIndex, Piece, NewBoard),
    switch_player(Player, NewPlayer).

replace([Row|RestRows], 0, ColIndex, Value, [NewRow|RestRows]) :-
    replace_in_row(Row, ColIndex, Value, NewRow).
replace([Row|RestRows], RowIndex, ColIndex, Value, [Row|NewRestRows]) :-
    RowIndex > 0,
    NextRowIndex is RowIndex - 1,
    replace(RestRows, NextRowIndex, ColIndex, Value, NewRestRows).

replace_in_row([_|RestCols], 0, Value, [Value|RestCols]).
replace_in_row([Col|RestCols], ColIndex, Value, [Col|NewRestCols]) :-
    ColIndex > 0,
    NextColIndex is ColIndex - 1,
    replace_in_row(RestCols, NextColIndex, Value, NewRestCols).

check_blocked_pieces(Board, FinalBoard) :-
    length(Board, BoardSize),
    findall(
        (X, Y),
        (
            aux_between(1, BoardSize, X),
            aux_between(1, BoardSize, Y),
            RowIndex is BoardSize - Y,
            ColIndex is X - 1,
            nthX(Board, RowIndex, Row),
            nthX(Row, ColIndex, Piece),
            Piece \= black,
            Piece \= empty,
            valid_moves([Board, Piece], (X, Y), [])
        ),
        BlockedPieces
    ),
    update_board(Board, BlockedPieces, FinalBoard).

update_board(Board, [], Board).
update_board(Board, [(X, Y) | T], NewBoard) :-
    length(Board, BoardSize),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    replace(Board, RowIndex, ColIndex, empty, TempBoard),
    update_board(TempBoard, T, NewBoard).

game_over([Board, _], draw) :-
  \+ (member(Row, Board), member(red, Row)),
  \+ (member(Row, Board), member(blue, Row)),
  !.

game_over([Board, _], blue) :-
    \+ (member(Row, Board), member(red, Row)),
    !.

game_over([Board, _], red) :-
    \+ (member(Row, Board), member(blue, Row)),
    !.
