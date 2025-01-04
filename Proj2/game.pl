:- use_module(library(lists)).
:- use_module(library(random)).

:- consult('menus.pl').
:- consult('displays.pl').
:- consult('utils.pl').
:- consult('logic.pl').

% play/0
play :- 
    nl, display_name, 
    main_menu.
