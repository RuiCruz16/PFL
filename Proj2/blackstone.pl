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
    nl, write('Starting Player vs Player game...'), nl.

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