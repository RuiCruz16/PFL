player(red, player1).
player(blue, player2).

switch_player(red, blue).
switch_player(blue, red).

initial_state([Board, red]) :-
    board(Board).

game_loop([Board, Player], _) :-
    game_over([Board, Player], draw),
    nl, write('Game over! It\'s a draw!'), nl.

game_loop([Board, Player], _) :-
    game_over([Board, Player], Winner),
    nl, format('Game over! The winner is ~w!~n', [Winner]).

game_loop([Board, Player], GameVariant) :-
    \+ game_over([Board, Player], _),
    display_game([Board, Player]),
    nl, choose_piece([Board, Player], PieceCoords),
    % format("Selected piece coordinates: ~w~n", [PieceCoords]).
    valid_moves([Board, Player], PieceCoords, ListOfMoves),
    format("Valid moves for selected piece: ~w~n", [ListOfMoves]),
    nl, choose_new_position(ListOfMoves, NewCoords),
    %format("New position coordinates: ~w~n", [NewCoords]),
    move([Board, Player], PieceCoords, NewCoords, [NewBoard, NewPlayer]),
    check_blocked_pieces(NewBoard, GameVariant, FinalBoard),
    game_loop([FinalBoard, NewPlayer], GameVariant).

game_loop_computer([Board, Player], _, _) :-
    game_over([Board, Player], draw),
    nl, write('Game over! It\'s a draw!'), nl.

game_loop_computer([Board, Player], _, _) :-
    game_over([Board, Player], Winner),
    nl, format('Game over! The winner is ~w!~n', [Winner]).

game_loop_computer([Board, Player], GameVariant, greedy) :- 
    \+ game_over([Board, Player], _),
    display_game([Board, Player]),
    nl, choose_piece([Board, Player], PieceCoords),
    % format("Selected piece coordinates: ~w~n", [PieceCoords]),
    valid_moves([Board, Player], PieceCoords, ListOfMoves),
    format("Valid moves for selected piece: ~w~n", [ListOfMoves]),
    nl, choose_new_position(ListOfMoves, NewCoords),
    % format("New position coordinates: ~w~n", [NewCoords]),
    move([Board, Player], PieceCoords, NewCoords, [NewBoard, NewPlayer]),
    check_blocked_pieces(NewBoard, GameVariant, TempBoard),
    perform_bot_move(TempBoard, NewPlayer, GameVariant, greedy, [FinalBoard, FinalPlayer]),
    game_loop_computer([FinalBoard, FinalPlayer], GameVariant, greedy).

perform_bot_move(Board, Player, GameVariant, Difficulty, [NewBoard, NewPlayer]) :-
    choose_move([Board, Player], Difficulty, [OldCoords, NewCoords]),
    move([Board, Player], OldCoords, NewCoords, [TempBoard, NewPlayer]),
    check_blocked_pieces(TempBoard, GameVariant, NewBoard).

choose_move([Board, Player], greedy, [OldCoords, NewCoords]) :-
    findall(
        [OldCoords, NewCoords, Value],
        (
            find_piece(Board, Player, OldCoords),
            valid_moves([Board, Player], OldCoords, ValidMoves),
            member(NewCoords, ValidMoves),
            simulate_move_and_evaluate(Board, Player, OldCoords, NewCoords, Value)
        ),
        MoveValues
    ),
    select_best_move(MoveValues, [OldCoords, NewCoords, _]),
    format("Bot chose move from ~w to ~w~n", [OldCoords, NewCoords]).

find_piece(Board, Player, (X, Y)) :-
    length(Board, BoardSize),
    aux_between(1, BoardSize, X),
    aux_between(1, BoardSize, Y),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    nthX(Board, RowIndex, Row),
    nthX(Row, ColIndex, Player).

simulate_move_and_evaluate(Board, Player, OldCoords, NewCoords, Value) :-
    move([Board, Player], OldCoords, NewCoords, [TempBoard, Opponent]),
    count_opponent_moves(TempBoard, Opponent, OpponentMoves),
    count_own_moves(TempBoard, Player, OwnMoves),
    evaluate_directions(TempBoard, Opponent, DirectionValue),
    WeightOpponentMoves is 1,
    WeightDirectionValue is 0.8,
    WeightOwnMoves is 0.2,
    Value is OpponentMoves * WeightOpponentMoves + DirectionValue * WeightDirectionValue - OwnMoves * WeightOwnMoves.

count_opponent_moves(Board, Opponent, TotalMoves) :-
    findall(
        (X, Y),
        find_piece(Board, Opponent, (X, Y)),
        OpponentPieces
    ),
    findall(
        Move,
        (
            member(Piece, OpponentPieces),
            valid_moves([Board, Opponent], Piece, Moves),
            member(Move, Moves)
        ),
        AllMoves
    ),
    length(AllMoves, TotalMoves).

count_own_moves(Board, Player, TotalMoves) :-
    findall(
        (X, Y),
        find_piece(Board, Player, (X, Y)),
        PlayerPieces
    ),
    findall(
        Move,
        (
            member(Piece, PlayerPieces),
            valid_moves([Board, Player], Piece, Moves),
            member(Move, Moves)
        ),
        AllMoves
    ),
    length(AllMoves, TotalMoves).

evaluate_directions(Board, Opponent, TotalDirections) :-
    findall(
        Directions,
        (
            find_piece(Board, Opponent, Piece),
            count_directions(Board, Opponent, Piece, Directions)
        ),
        DirectionValues
    ),
    aux_sumlist(DirectionValues, TotalDirections).

aux_sumlist([], 0).
aux_sumlist([Head | Tail], Sum) :-
    aux_sumlist(Tail, PartialSum), % Soma parcial do restante da lista.
    Sum is Head + PartialSum. % Soma o elemento atual à soma parcial.

count_directions(Board, _, (X, Y), DirectionCount) :-
    length(Board, BoardSize),
    findall(
        1,
        (
            direction(DX, DY),
            NRow is X + DX,
            NCol is Y + DY,
            within_bounds(NRow, NCol, BoardSize),
            nthX(Board, NRow, Row),
            nthX(Row, NCol, Cell),
            Cell = empty
        ),
        Directions
    ),
    length(Directions, DirectionCount).

select_best_move(MoveValues, BestMove) :-
    sort_moves(MoveValues, SortedMoves), % Ordenar por valor (menor primeiro)
    write('Sorted moves: '), write(SortedMoves), nl,
    nth0(0, SortedMoves, BestMove).        % Escolher o primeiro movimento da lista

% Função personalizada para ordenar os movimentos
sort_moves(MoveValues, SortedMoves) :-
    quicksort(MoveValues, SortedMoves).

% Implementação do quicksort
quicksort([], []).
quicksort([H|T], Sorted) :-
    partition(H, T, L, R),
    quicksort(L, SortedL),
    quicksort(R, SortedR),
    append(SortedL, [H|SortedR], Sorted).

% Particiona a lista em duas, com base no valor do terceiro elemento
partition(_, [], [], []).
partition([A, B, V], [[C, D, W]|T], [[C, D, W]|L], R) :-
    W =< V,
    partition([A, B, V], T, L, R).
partition([A, B, V], [[C, D, W]|T], L, [[C, D, W]|R]) :-
    W > V,
    partition([A, B, V], T, L, R).

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

check_blocked_pieces(Board, default, FinalBoard) :-
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

check_blocked_pieces(Board, medium_churn, FinalBoard) :-
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
    update_board(Board, BlockedPieces, TempBoard),
    findall(
      (BX, BY),
      (
      member((X, Y), BlockedPieces),
      direction(DX, DY),
      BX is X + DX,
      BY is Y + DY,
      within_bounds(BoardSize - BY, BX - 1, BoardSize),
      RowIndex is BoardSize - BY,
      ColIndex is BX - 1,
      nthX(TempBoard, RowIndex, Row),
      nthX(Row, ColIndex, black)
      ),
      AdjacentBlackPieces
    ),
    update_board(TempBoard, AdjacentBlackPieces, FinalBoard).

check_blocked_pieces(Board, high_churn, FinalBoard) :-
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
    handle_high_churn(Board, BlockedPieces, FinalBoard, BoardSize).

handle_high_churn(Board, [], Board, _) :- !.
handle_high_churn(Board, BlockedPieces, FinalBoard, BoardSize) :-
    update_board(Board, BlockedPieces, TempBoard),
    findall(
        (BX, BY),
        (
            aux_between(1, BoardSize, BX),
            aux_between(1, BoardSize, BY),
            RowIndex is BoardSize - BY,
            ColIndex is BX - 1,
            nthX(TempBoard, RowIndex, Row),
            nthX(Row, ColIndex, black)
        ),
        AllBlackPieces
    ),
    update_board(TempBoard, AllBlackPieces, FinalBoard).

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
