% Auxiliary predicates
player(red, player1).
player(blue, player2).

% Predicate to switch the player
switch_player(red, blue).
switch_player(blue, red).

% Predicate to select the difficulty of the corresponding computer player
select_difficulty(red, [DifficultyPC1, _], DifficultyPC1).
select_difficulty(blue, [_, DifficultyPC2], DifficultyPC2).

% initial_state(+BoardSize, -GameState)
% Initializes the game state based on the given board size
% The predicate succeeds by generating a board of the specified size and setting the initial player to red
initial_state(BoardSize, [Board, red]) :-
    board(BoardSize, Board).

% game_loop(+GameState, +GameVariant)
% Main game loop of the Player vs Player game mode, taking into account the game variant
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
    valid_moves([Board, Player], PieceCoords, ListOfMoves),
    format("Valid moves for selected piece: ~w~n", [ListOfMoves]),
    nl, choose_new_position(ListOfMoves, NewCoords),
    move([Board, Player], PieceCoords, NewCoords, [NewBoard, NewPlayer]),
    check_blocked_pieces(NewBoard, GameVariant, FinalBoard),
    nl, write('Player '), write(Player), write(' moved from ('), write(PieceCoords), write(') to ('), write(NewCoords), write(')'), nl,
    game_loop([FinalBoard, NewPlayer], GameVariant).

% game_loop_player_pc(+GameState, +GameVariant, +Difficulty, +FirstPlayer)
% Main game loop of the Player vs Computer game mode, taking into account the game variant, the difficulty chosen to the computer player and the first player to play
game_loop_player_pc([Board, Player], _, _, _) :-
    game_over([Board, Player], draw),
    nl, write('Game over! It\'s a draw!'), nl.

game_loop_player_pc([Board, Player], _, _, _) :-
    game_over([Board, Player], Winner),
    nl, format('Game over! The winner is ~w!~n', [Winner]).

game_loop_player_pc([Board, Player], GameVariant, Difficulty, player_first) :- 
    \+ game_over([Board, Player], _),
    display_game([Board, Player]),
    nl, choose_piece([Board, Player], PieceCoords),
    valid_moves([Board, Player], PieceCoords, ListOfMoves),
    format("Valid moves for selected piece: ~w~n", [ListOfMoves]),
    nl, choose_new_position(ListOfMoves, NewCoords),
    move([Board, Player], PieceCoords, NewCoords, [NewBoard, NewPlayer]),
    check_blocked_pieces(NewBoard, GameVariant, TempBoard),
    nl, write('Player ('), write(Player), write(') moved from ('), write(PieceCoords), write(') to ('), write(NewCoords), write(')'), nl,
    display_game([TempBoard, NewPlayer]), nl, 
    perform_computer_move([TempBoard, NewPlayer], GameVariant, Difficulty, [FinalBoard, FinalPlayer]),
    game_loop_player_pc([FinalBoard, FinalPlayer], GameVariant, Difficulty, player_first).

game_loop_player_pc([Board, Player], GameVariant, Difficulty, pc_first) :-
    \+ game_over([Board, Player], _),
    display_game([Board, Player]),
    nl, perform_computer_move([Board, Player], GameVariant, Difficulty, [NewBoard, NewPlayer]),
    display_game([NewBoard, NewPlayer]), nl,
    nl, choose_piece([NewBoard, NewPlayer], PieceCoords),
    valid_moves([NewBoard, NewPlayer], PieceCoords, ListOfMoves),
    format("Valid moves for selected piece: ~w~n", [ListOfMoves]),
    nl, choose_new_position(ListOfMoves, NewCoords),
    move([NewBoard, NewPlayer], PieceCoords, NewCoords, [TempBoard, FinalPlayer]),
    check_blocked_pieces(TempBoard, GameVariant, FinalBoard),
    nl, write('Player ('), write(NewPlayer), write(') moved from ('), write(PieceCoords), write(') to ('), write(NewCoords), write(')'), nl,
    game_loop_player_pc([FinalBoard, FinalPlayer], GameVariant, Difficulty, pc_first).

% game_loop_pc_pc(+GameState, +GameVariant, +Difficulties)
% Main game loop of the Computer vs Computer game mode, taking into account the game variant 
% and the difficulties chosen to the computer players (there can be different difficulties for each player)
game_loop_pc_pc([Board, Player], _, _) :-
    game_over([Board, Player], draw),
    nl, write('Game over! It\'s a draw!'), nl.

game_loop_pc_pc([Board, Player], _, _) :-
    game_over([Board, Player], Winner),
    nl, format('Game over! The winner is ~w!~n', [Winner]).

game_loop_pc_pc([Board, Player], GameVariant, [DifficultyPC1, DifficultyPC2]) :-
    \+ game_over([Board, Player], _),
    display_game([Board, Player]),
    approve_input(Player),
    select_difficulty(Player, [DifficultyPC1, DifficultyPC2], Difficulty),
    perform_computer_move([Board, Player], GameVariant, Difficulty, [NewBoard, NewPlayer]),
    game_loop_pc_pc([NewBoard, NewPlayer], GameVariant, [DifficultyPC1, DifficultyPC2]).

% approve_input(+Player)
% Predicate to wait for the user to press any key to continue the game and perform the computer move
approve_input(Player) :-
    nl, write('Press any key to continue...'), nl,
    read_line(_),
    nl, write('Player '), write(Player), write(' turn.'), nl.

% perform_computer_move(+GameState, +GameVariant, +Difficulty, -NewGameState)
% Predicate to determine and execute the computer's move based on the current game state, 
% the variant of the game being played, and the chosen difficulty level
% The resulting move should be applied to the GameState, updating it accordingly
perform_computer_move([Board, Player], GameVariant, Difficulty, [NewBoard, NewPlayer]) :-
    choose_move([Board, Player], Difficulty, [ChosenPosition, NewPosition]),
    move([Board, Player], ChosenPosition, NewPosition, [TempBoard, NewPlayer]),
    check_blocked_pieces(TempBoard, GameVariant, NewBoard).

% choose_move(+GameState, +Difficulty, -Move)
% Predicate to choose the next move of the computer based on the current game state and the chosen difficulty level
% In the case of the greedy difficulty, the computer will simulate all the available moves and choose the one that has the lowest value,
% because the value is calculated based on the number of moves, directions that the opponent can make and the number of moves that the computer can make,
% a lower value means that the computer was able to reduce the opponent's moves and directions and increase its own moves
% In the case of the random difficulty, the computer will choose a random piece to move and a random position to move to
choose_move([Board, Player], greedy, [ChosenPosition, NewPosition]) :-
    findall(
        [ChosenPosition, NewPosition, Value],
        (
            find_piece(Board, Player, ChosenPosition),
            valid_moves([Board, Player], ChosenPosition, ValidMoves),
            member(NewPosition, ValidMoves),
            simulate_move([Board, Player], ChosenPosition, NewPosition, Value)
        ),
        MoveValues
    ),
    select_best_move(MoveValues, [ChosenPosition, NewPosition, _]),
    format("Computer (~w) chose move from (~w) to (~w)~n", [Player, ChosenPosition, NewPosition]).

choose_move([Board, Player], random, [ChosenPosition, NewPosition]) :-
    findall(
        ChosenPosition,
        (
            find_piece(Board, Player, ChosenPosition)
        ),
        ListOfPieces
    ), 
    random_member(ChosenPosition, ListOfPieces),
    valid_moves([Board, Player], ChosenPosition, ListOfMoves),
    random_member(NewPosition, ListOfMoves),
    format("Computer (~w) chose move from (~w) to (~w)~n", [Player, ChosenPosition, NewPosition]).

% find_piece(+Board, +Player, -Piece)
% Predicate to find a piece of the specified player on the given board
find_piece(Board, Player, (X, Y)) :-
    length(Board, BoardSize),
    aux_between(1, BoardSize, X),
    aux_between(1, BoardSize, Y),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    nthX(Board, RowIndex, Row),
    nthX(Row, ColIndex, Player).

% simulate_move(+GameState, +ChosenPosition, +NewPosition, -Value)
% Predicate to simulate a move taking into account the chosen position and the new position, returning the value of the move
simulate_move([Board, Player], ChosenPosition, NewPosition, Value) :-
    move([Board, Player], ChosenPosition, NewPosition, [TempBoard, Opponent]),
    value([TempBoard, Player], Opponent, Value).

% value(+GameState, +Player, -Value)
% Predicate to calculate the value of a move based on the current game state.
% This is used to evaluate the value of the move previously simulated by the bot
% to determine the best possible move
value([Board, Player], Opponent, Value) :-
    count_player_moves(Board, Opponent, OpponentMoves),
    count_player_moves(Board, Player, ComputerMoves),
    count_all_directions(Board, Opponent, OpponentDirections),
    WeightOpponentMoves is 1,
    WeightOpponentDirections is 0.8,
    WeightComputerMoves is 0.2,
    Value is OpponentMoves * WeightOpponentMoves + OpponentDirections * WeightOpponentDirections - ComputerMoves * WeightComputerMoves.

% count_player_moves(+Board, +Player, -TotalMoves)
% Predicate to count the total number of moves that a player can make on the given board
count_player_moves(Board, Player, TotalMoves) :-
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

% count_all_directions(+Board, +Player, -TotalDirections)
% Predicate to count the total number of directions that a player can move on the given board
count_all_directions(Board, Player, TotalDirections) :-
    findall(
        PieceDirections,
        (
            find_piece(Board, Player, Piece),
            count_piece_directions(Board, Piece, PieceDirections)
        ),
        DirectionValues
    ),
    aux_sumlist(DirectionValues, TotalDirections).

% count_piece_directions(+Board, +Piece, -DirectionsCount)
% Predicate to count the number of directions of a specific piece on the given board
count_piece_directions(Board, (X, Y), DirectionsCount) :-
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
    length(Directions, DirectionsCount).

% select_best_move(+MoveValues, -BestMove)
% Predicate to select the best move from a list of moves based on the value of each move
% The best move is the one with the lowest value
select_best_move(MoveValues, BestMove) :-
    sort_moves(MoveValues, SortedMoves),
    nthX(SortedMoves, 0, BestMove).

% sort_moves(+MoveValues, -SortedMoves)
% Predicate to sort the moves based on the value of each move in ascending order
sort_moves(MoveValues, SortedMoves) :-
    quicksort(MoveValues, SortedMoves).

% choose_piece(+GameState, -PieceCoords)
% Predicate that prompts the user to choose a piece to move
choose_piece([Board, Player], Coords) :-
    write('Select a piece to move'), nl,
    write('Enter X coordinate: '),
    read_input_number(X),
    write('Enter Y coordinate: '),
    read_input_number(Y),
    length(Board, BoardSize),
    validate_coordinates(X, Y, BoardSize, [Board, Player], Coords).

% validate_coordinates(+X, +Y, +BoardSize, +GameState, -Coords)
% Predicate to validate if the coordinates chosen by the user are within the bounds of the board
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

% validate_piece(+RowIndex, +ColIndex, +Piece, +Player, +BoardSize, +GameState, -Coords)
% Predicate to validate if the piece chosen by the user is valid (belongs to the player and is not black or empty)
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

% valid_moves(+GameState, +PieceCoords, -ListOfMoves)
% Predicate to find all the valid moves of a given piece taking into account the current game state
valid_moves([Board, Player], (X, Y), ListOfMoves) :-
    length(Board, BoardSize),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    findall(
        (NX, NY),
        (valid_move([Board, Player], RowIndex, ColIndex, NX, NY)),
        ListOfMoves
    ).

% valid_move(+GameState, +RowIndex, +ColIndex, -NX, -NY)
% Predicate to determine all valid moves for a piece in every possible direction
valid_move([Board, _], RowIndex, ColIndex, NX, NY) :-
    length(Board, BoardSize),
    direction(DX, DY),
    generate_moves(RowIndex, ColIndex, DX, DY, Board, BoardSize, NX, NY).

% direction(+DX, +DY)
% Predicate to determine all the possible directions that a piece can move
direction(-1, 0).
direction(1, 0).
direction(0, -1).
direction(0, 1).
direction(-1, -1).
direction(-1, 1).
direction(1, -1).
direction(1, 1).

% generate_moves(+Row, +Col, +DX, +DY, +Board, +BoardSize, -NX, -NY)
% Predicate to generate recursively all the possible moves of a piece in a given direction, until it reaches the end of the board or finds a piece
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

% within_bounds(+Row, +Col, +BoardSize)
% Predicate to determine if a given position is within the bounds of the board
within_bounds(Row, Col, BoardSize) :-
    Row >= 0, Row < BoardSize,
    Col >= 0, Col < BoardSize.

% choose_new_position(+ListOfMoves, -NewCoords)
% Predicate to prompt the user to choose a new position to move the piece
choose_new_position(ListOfMoves, NewCoords) :-
    write('Select a new position to move the piece'), nl,
    write('Enter X coordinate: '),
    read_input_number(X),
    write('Enter Y coordinate: '),
    read_input_number(Y),
    validate_new_position(X, Y, ListOfMoves, NewCoords).

% validate_new_position(+X, +Y, +ListOfMoves, -NewCoords)
% Predicate to validate if the new position chosen by the user is within the list of valid moves
validate_new_position(X, Y, ListOfMoves, (X, Y)) :-
  member((X, Y), ListOfMoves).

validate_new_position(X, Y, ListOfMoves, NewCoords) :-
    \+ member((X, Y), ListOfMoves),
    write('Invalid move. The selected coordinates are not in the list of valid moves. Please try again.'), nl,
    nl, choose_new_position(ListOfMoves, NewCoords).

% move(+GameState, +PieceCoords, +NewCoords, -NewGameState)
% Predicate to move a piece from the given coordinates to the new coordinates and update the game state accordingly
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

% replace(+Board, +RowIndex, +ColIndex, +Value, -NewBoard)
% Predicate to replace a piece in the board at the specified coordinates with a new piece
replace([Row|RestRows], 0, ColIndex, Value, [NewRow|RestRows]) :-
    replace_in_row(Row, ColIndex, Value, NewRow).
replace([Row|RestRows], RowIndex, ColIndex, Value, [Row|NewRestRows]) :-
    RowIndex > 0,
    NextRowIndex is RowIndex - 1,
    replace(RestRows, NextRowIndex, ColIndex, Value, NewRestRows).

% replace_in_row(+Row, +ColIndex, +Value, -NewRow)
% Predicate to replace a piece in a row at the specified index with a new piece
replace_in_row([_|RestCols], 0, Value, [Value|RestCols]).
replace_in_row([Col|RestCols], ColIndex, Value, [Col|NewRestCols]) :-
    ColIndex > 0,
    NextColIndex is ColIndex - 1,
    replace_in_row(RestCols, NextColIndex, Value, NewRestCols).

% check_blocked_pieces(+Board, +GameVariant, -FinalBoard)
% Predicate to check if there are any blocked pieces on the board and update the board accordingly
% In the default game variant, only the blocked pieces (the ones that have no valid moves) are removed from the board
% In the medium churn game variant, the blocked pieces are removed from the board and all the surrounding black pieces are also removed
% In the high churn game variant, the blocked pieces are removed from the board and all the black pieces are also removed
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

% handle_high_churn(+Board, +BlockedPieces, -FinalBoard, +BoardSize)
% Auxiliary predicate to handle the high churn game variant
% Finds all the black pieces on the given board and removes them
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

% update_board(+Board, +Pieces, -NewBoard)
% Predicate to update the board by removing the specified pieces and replacing them with empty pieces
update_board(Board, [], Board).
update_board(Board, [(X, Y) | T], NewBoard) :-
    length(Board, BoardSize),
    RowIndex is BoardSize - Y,
    ColIndex is X - 1,
    replace(Board, RowIndex, ColIndex, empty, TempBoard),
    update_board(TempBoard, T, NewBoard).

% game_over(+GameState, -Winner)
% Predicate to determine if the game is over (when one or both of the players has no pieces left on the board)
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
