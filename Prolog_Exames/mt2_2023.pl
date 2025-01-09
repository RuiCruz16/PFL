:- use_module(library(lists)).

% dish(Name, Price, IngredientGrams).
dish(pizza,         2200, [cheese-300, tomato-350]).
dish(ratatouille,   2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread,  1600, [cheese-50, garlic-200]).

:- dynamic ingredient/2.

% ingredient(Name, CostPerGram).
ingredient(cheese,   4).
ingredient(tomato,   2).
ingredient(eggplant, 7).
ingredient(garlic,   6).

count_ingredients(Dish, NumIngredients) :-
    dish(Dish, _, IngredientsList),
    length(IngredientsList, NumIngredients).

ingredient_amount_cost(Ingredient, Grams, TotalCost) :-
    ingredient(Ingredient, CostPerGram),
    TotalCost is Grams * CostPerGram.

dish_profit(Dish, Profit) :-
    dish(Dish, Price, IngredientsList),
    calculate_ingredients_price(IngredientsList, 0, IngredientsPrice),
    Profit is Price - IngredientsPrice.

calculate_ingredients_price([], IngredientsPrice, IngredientsPrice).
calculate_ingredients_price([(Ing-IngGram) | T], Acc, IngredientsPrice) :-
    ingredient_amount_cost(Ing, IngGram, Cost),
    Acc1 is Acc + Cost,
    calculate_ingredients_price(T, Acc1, IngredientsPrice).

update_unit_cost(Ingredient, NewUnitCost) :-
    ingredient(Ingredient, _),
    retract(ingredient(Ingredient, _)),
    asserta(ingredient(Ingredient, NewUnitCost)).

update_unit_cost(Ingredient, NewUnitCost) :-
    \+ ingredient(Ingredient, _),
    asserta(ingredient(Ingredient, NewUnitCost)).

most_expensive_dish(Dish, Price) :-
    dish(Dish, Price, _),
    \+ (dish(_, Price2, _), Price2 > Price).

consume_ingredient(IngredientStocks, Ingredient, Grams, NewIngredientStocks) :-
    append(Aux, [(Ingredient-TempGrams) | Rest], IngredientStocks),
    TempGrams > Grams,
    FinalGrams is TempGrams - Grams,
    append(Aux, [(Ingredient-FinalGrams) | Rest], NewIngredientStocks).

count_dishes_with_ingredient(Ingredient, N) :-
    collect_all_dishes([], DishList),
    !,
    check_ingredient_into_dishes(Ingredient, DishList, 0, N).

collect_all_dishes(Acc, DishList) :-
    dish(Dish, _, IngredientsList),
    \+ memberchk(Dish-IngredientsList, Acc),
    collect_all_dishes([Dish-IngredientsList | Acc], DishList).
collect_all_dishes(DishList, DishList).

check_ingredient_into_dishes(_, [], N, N).
check_ingredient_into_dishes(Ingredient, [(_-IngredientsList) | T], Acc, N) :-
    check_dish(Ingredient, IngredientsList, Aux),
    Acc1 is Acc + Aux,
    check_ingredient_into_dishes(Ingredient, T, Acc1, N).    

check_dish(Ingredient, [(Ingredient-_) | _], Aux) :-
    Aux = 1.

check_dish(_, [], 0).

check_dish(Ingredient, [ _ | T], Aux) :-
    check_dish(Ingredient, T, Aux).

list_dishes(DishIngredients) :-
    collect_all_dishes([], DishList),
    pair_dish_list(DishList, [], DishIngredients).

pair_dish_list([], DishIngredients, DishIngredients).
pair_dish_list([(Dish-IngredientsList) | T], Acc, DishIngredients) :-
    collect_only_ingredients(IngredientsList, [], AuxList),
    append([Dish-AuxList], Acc, Acc1),
    pair_dish_list(T, Acc1, DishIngredients).

collect_only_ingredients([], AuxList, AuxList).
collect_only_ingredients([(Ing-_) | T], Acc, AuxList) :-
    append(Acc, [Ing], Acc1),
    collect_only_ingredients(T, Acc1, AuxList).

most_lucrative_dishes(Dishes) :-
    findall(Profit-Dish, dish_profit(Dish, Profit), DishesProfit),
    keysort(DishesProfit, TempList),
    reverse(TempList, SortedList),
    collect_only_dishes(SortedList, [], Dishes).

collect_only_dishes([], Dishes, Dishes).
collect_only_dishes([(_-Dish) | T], Acc, Dishes) :-
    append(Acc, [Dish], Acc1),
    collect_only_dishes(T, Acc1, Dishes).

%G1
edge(g1, br, o).
edge(g1, br, ni).
edge(g1, o, ni).
edge(g1, o, c).
edge(g1, o, h).
edge(g1, h, c).
edge(g1, h, n).
edge(g1, n, he).
edge(g1, c, he).

% G2
edge(g2, br, h).
edge(g2, br, ni).
edge(g2, h, ni).
edge(g2, h, o).
edge(g2, h, c).
edge(g2, o, c).
edge(g2, o, n).
edge(g2, n, he).
edge(g2, c, he).
edge(g2, cl, he).

% Para X-Y == Y-X
con(G, X-Y) :-
    edge(G, X, Y).
con(G, X-Y) :-
    edge(G, Y, X).

common_edges(G1, G2, L) :-
    findall(V1-V2, (edge(G1, V1, V2), con(G2, V1-V2)), L).

common_subgraphs(G1, G2, Subgraphs):-
    common_edges(G1, G2, Edges),
    common_subgraphs_aux(Edges, Subgraphs).

common_subgraphs_aux([], []).
common_subgraphs_aux([V1-V2|Es], [SGNoDups|SGs]):-
    next_subgraph([V1,V2], Es, NewEs, SG),
    sort(SG, SGNoDups), % remove duplicate nodes
    common_subgraphsaux(NewEs, SGs).

adjacent(V, V-).
adjacent(V, _-V).

next_subgraph(Vs, Es, Es, Vs):-
    +((
        member(V, Vs),
        select(E, Es, ),
        adjacent(V, E)
    )), !.
next_subgraph(Vs, Es, NewEs, SG):-
    member(V, Vs),
    select(V1-V2, Es, Es1),
    adjacent(V, V1-V2),
    !,
    next_subgraph([V1,V2|Vs], Es1, NewEs, SG).
