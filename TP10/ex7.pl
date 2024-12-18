% Caso base: 0 degraus, 1 forma (nenhum passo).
steps(0, 1, [[]]).

% Caso base: 1 degrau, 1 forma ([1]).
steps(1, 1, [[1]]).

% Caso geral: recursão para N > 1.
steps(N, Total, Possibilidades) :-
    N > 1,
    N1 is N - 1, steps(N1, Total1, Poss1),
    N2 is N - 2, steps(N2, Total2, Poss2),
    Total is Total1 + Total2,
    adicionar_passo(Poss1, 1, Poss1ComUm),
    adicionar_passo(Poss2, 2, Poss2ComDois),
    append(Poss1ComUm, Poss2ComDois, Possibilidades).

% Adiciona um passo (1 ou 2) no início de cada sequência.
adicionar_passo([], _, []).
adicionar_passo([H | T], Passo, [[Passo | H] | Resto]) :-
    adicionar_passo(T, Passo, Resto).

% ?- steps(3, Total, Possibilidades).
% Total = 3,
% Possibilidades = [[1,1,1], [1,2], [2,1]].
