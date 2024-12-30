% Displays the main menu of the game
main_menu :- 
    write('1. Start Game'), nl,
    write('2. Instructions'), nl,
    write('3. Exit'), nl,
    write('Please select an option: '),
    read_input_number(MenuOption),
    handle_main_menu_input(MenuOption).

% Handles the input of the main menu
handle_main_menu_input(1) :- 
    nl, game_menu.

handle_main_menu_input(2) :-
    nl, instructions_menu.

handle_main_menu_input(3) :-
    nl, write('Exiting game...'), !.

handle_main_menu_input(_) :-
    nl, write('Invalid option, try again.'), nl,
    main_menu.

% Displays the instructions of the game
instructions_menu :-
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
    main_menu.

% Selects the game variant and saves it for later use
select_game_variant(GameVariant) :-
  nl, write('=== Game Variant Selection ==='), nl,
  write('1. Default -> Play with the default rules of the game.'), nl,
  write('2. Medium Churn -> When a piece is removed, all surrounding black pieces are also removed.'), nl,
  write('3. High Churn -> When a piece is removed, all black pieces on the board are also removed.'), nl,
  write('Please select a game variant (1-3): '),
  read_input_number(GameOption),
  handle_game_variant_input(GameOption, GameVariant).

handle_game_variant_input(1, default) :-
  nl, write('Default variant selected.'), nl.
handle_game_variant_input(2, medium_churn) :-
  nl, write('Medium Churn variant selected.'), nl.
handle_game_variant_input(3, high_churn) :-
  nl, write('High Churn variant selected.'), nl.
handle_game_variant_input(_, GameVariant) :-
  write('Invalid option. Try again.'), nl,
  select_game_variant(GameVariant).

% Displays the game menu
game_menu :-
    write('Select the mode you want to play:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Return to main menu'), nl,
    write('Please select an option: '),
    read_input_number(GameOption),
    handle_game_menu_input(GameOption).

% Handles the input of the game menu
handle_game_menu_input(1) :-
  select_game_variant(GameVariant),
  nl, write('Starting Player vs Player game...'), nl,
  initial_state(GameState),
  game_loop(GameState, GameVariant).

% TODO: Implementar modo de jogo contra o computador
handle_game_menu_input(2) :-
    nl, write('Coming soon...').

% TODO: Implementar modo de jogo entre computadores
handle_game_menu_input(3) :-
    nl, write('Coming soon...').

handle_game_menu_input(4) :-
    nl, write('Returning to main menu...'), nl,
    main_menu.

handle_game_menu_input(_) :-
    nl, write('Invalid option, try again.'), nl,
    game_menu.
