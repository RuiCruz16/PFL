:- use_module(library(lists)).
:- use_module(library(random)).

:- consult('menus.pl').
:- consult('displays.pl').
:- consult('utils.pl').
:- consult('logic.pl').

% play/0
% Main predicate to start the game, give acess to the main menu, where the user can play the game
play :- 
    nl, display_name, 
    main_menu.
