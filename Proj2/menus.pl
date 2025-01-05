% main_menu/0
% Displays the main menu of the game
main_menu :-
    write('=== Main Menu ==='), nl,
    write('1. Start Game'), nl,
    write('2. Instructions'), nl,
    write('3. Exit'), nl,
    write('Please select an option: '),
    read_input_number(MenuOption),
    handle_main_menu_input(MenuOption).

% handle_main_menu_input(+MenuOption)
% Handles the input of the main menu
handle_main_menu_input(1) :- 
    nl, game_menu.

handle_main_menu_input(2) :-
    nl, instructions_menu.

handle_main_menu_input(3) :-
    nl, write('Exiting game...'), nl, !, abort.

handle_main_menu_input(_) :-
    nl, write('Invalid option, try again.'), nl,
    main_menu.

% instructions_menu/0
% Displays the instructions of the game
instructions_menu :-
    write('=== Blackstone Instructions ==='), nl,
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
    nl, main_menu.

% select_game_variant(-GameVariant)
% Displays the game variant selection menu to help the user choose the game variant
select_game_variant(GameVariant) :-
  nl, write('=== Game Variant Selection ==='), nl,
  write('1. Default -> Play with the default rules of the game.'), nl,
  write('2. Medium Churn -> When a piece is removed, all surrounding black pieces are also removed.'), nl,
  write('3. High Churn -> When a piece is removed, all black pieces on the board are also removed.'), nl,
  write('Please select a game variant (1-3): '),
  read_input_number(GameOption),
  handle_game_variant_input(GameOption, GameVariant).

% handle_game_variant_input(+GameOption, -GameVariant)
% Handles the input of the game variant menu
handle_game_variant_input(1, default) :-
  nl, write('Default variant selected.'), nl.
handle_game_variant_input(2, medium_churn) :-
  nl, write('Medium Churn variant selected.'), nl.
handle_game_variant_input(3, high_churn) :-
  nl, write('High Churn variant selected.'), nl.
handle_game_variant_input(_, GameVariant) :-
  write('Invalid option. Try again.'), nl,
  select_game_variant(GameVariant).

% select_board_size(-BoardSize)
% Displays the board size selection menu to help the user choose the board size
select_board_size(BoardSize) :-
    nl, write('=== Board Size Selection ==='), nl,
    write('1. 6x6'), nl,
    write('2. 8x8'), nl,
    write('3. 10x10'), nl,
    write('Please select a board size (1-3): '),
    read_input_number(BoardSizeOption),
    handle_board_size_input(BoardSizeOption, BoardSize).

% handle_board_size_input(+BoardSizeOption, -BoardSize)
% Handles the input of the board size menu
handle_board_size_input(1, 6) :-
    nl, write('6x6 board selected.'), nl.

handle_board_size_input(2, 8) :-
    nl, write('8x8 board selected.'), nl.

handle_board_size_input(3, 10) :-
    nl, write('10x10 board selected.'), nl.

handle_board_size_input(_, BoardSize) :-
    write('Invalid option. Try again.'), nl,
    select_board_size(BoardSize).

% select_computer_difficulty(-Difficulty)
% Displays the computer difficulty selection menu to help the user choose the computer difficulty
select_computer_difficulty(Difficulty) :-
    nl, write('=== Computer Difficulty Selection ==='), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    write('Please select a difficulty (1-2): '),
    read_input_number(DifficultyOption),
    handle_computer_difficulty_input(DifficultyOption, Difficulty).

% handle_computer_difficulty_input(+DifficultyOption, -Difficulty)
% Handles the input of the computer difficulty menu
handle_computer_difficulty_input(1, random) :-
    nl, write('Random difficulty selected.'), nl.

handle_computer_difficulty_input(2, greedy) :-
    nl, write('Greedy difficulty selected.'), nl.

handle_computer_difficulty_input(_, Difficulty) :-
    write('Invalid option. Try again.'), nl,
    select_computer_difficulty(Difficulty).

% game_menu/0
% Displays the game menu
game_menu :-
    write('Select the mode you want to play:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Player'), nl,
    write('4. Computer vs Computer'), nl,
    write('5. Return to main menu'), nl,
    write('Please select an option: '),
    read_input_number(GameOption),
    handle_game_menu_input(GameOption).

% handle_game_menu_input(+GameOption)
% Handles the input of the game menu
handle_game_menu_input(1) :-
    select_board_size(BoardSize),
    select_game_variant(GameVariant),
    nl, write('Starting Player vs Player game...'), nl,
    initial_state(BoardSize, GameState),
    game_loop(GameState, GameVariant).

handle_game_menu_input(2) :-
    select_board_size(BoardSize),
    select_computer_difficulty(Difficulty),
    select_game_variant(GameVariant),
    nl, write('Starting Player vs Computer game...'), nl,
    initial_state(BoardSize, GameState),
    game_loop_player_pc(GameState, GameVariant, Difficulty, player_first).

handle_game_menu_input(3) :-
    select_board_size(BoardSize),
    select_computer_difficulty(Difficulty),
    select_game_variant(GameVariant),
    nl, write('Starting Computer vs Player game...'), nl,
    initial_state(BoardSize, GameState),
    game_loop_player_pc(GameState, GameVariant, Difficulty, pc_first).

handle_game_menu_input(4) :-
    select_board_size(BoardSize),
    nl, write('Select the difficulty for the red side computer:'), nl,
    select_computer_difficulty(DifficultyPC1),
    nl, write('Select the difficulty for the blue side computer:'), nl,
    select_computer_difficulty(DifficultyPC2),
    select_game_variant(GameVariant),
    nl, write('Starting Computer vs Computer game...'), nl,
    initial_state(BoardSize, GameState),
    game_loop_pc_pc(GameState, GameVariant, [DifficultyPC1, DifficultyPC2]).

handle_game_menu_input(5) :-
    nl, write('Returning to main menu...'), nl,
    main_menu.

handle_game_menu_input(_) :-
    nl, write('Invalid option, try again.'), nl,
    game_menu.
