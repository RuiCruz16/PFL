% Alínea B)

fold(_, FinalValue, [], FinalValue).

fold(Pred, StartValue, [H | T], FinalValue) :-
    call(Pred, StartValue, H, IntermediateValue),
    fold(Pred, IntermediateValue, T, FinalValue).

% Alínea C)

separate([], _, [], []).

separate([H|T], Pred, [H|Yes], No) :-
    call(Pred, H),
    separate(T, Pred, Yes, No).

separate([H|T], Pred, Yes, [H|No]) :-
    \+ call(Pred, H),
    separate(T, Pred, Yes, No).
