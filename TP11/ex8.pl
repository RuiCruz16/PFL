% Alínea A)

:- op(700, xfx, exists_in). % 'xfx' makes it non-associative.

Element exists_in List :-
    member(Element, List).
