% Estado representado como estado(ME, CE, MD, CD, Lado).
% ME: Missionários na margem esquerda.
% CE: Canibais na margem esquerda.
% MD: Missionários na margem direita.
% CD: Canibais na margem direita.
% Lado: lado atual do barco (esquerda ou direita).

% Estado inicial e final.
estado_inicial(estado(3, 3, 0, 0, esquerda)).
estado_final(estado(0, 0, 3, 3, _)).

% Movimentos possíveis: (Missionários, Canibais).
movimento(1, 0).
movimento(2, 0).
movimento(0, 1).
movimento(0, 2).
movimento(1, 1).

% Verifica se o estado é válido.
estado_valido(estado(ME, CE, MD, CD, _)) :-
    ME >= 0, CE >= 0, MD >= 0, CD >= 0, % Nenhuma margem negativa.
    (ME = 0; ME >= CE),                 % Missionários >= canibais na margem esquerda.
    (MD = 0; MD >= CD).                 % Missionários >= canibais na margem direita.

% Transição de estados (esquerda para direita).
transicao(estado(ME, CE, MD, CD, esquerda), estado(MEN, CEN, MDN, CDN, direita), (M, C)) :-
    movimento(M, C),
    MEN is ME - M, CEN is CE - C,
    MDN is MD + M, CDN is CD + C,
    estado_valido(estado(MEN, CEN, MDN, CDN, direita)).

% Transição de estados (direita para esquerda).
transicao(estado(ME, CE, MD, CD, direita), estado(MEN, CEN, MDN, CDN, esquerda), (M, C)) :-
    movimento(M, C),
    MEN is ME + M, CEN is CE + C,
    MDN is MD - M, CDN is CD - C,
    estado_valido(estado(MEN, CEN, MDN, CDN, esquerda)).

% Busca para encontrar a solução.
missionarios_e_canibais(Solucao) :-
    estado_inicial(EstadoInicial),
    estado_final(EstadoFinal),
    busca(EstadoInicial, EstadoFinal, [EstadoInicial], Solucao).

% Caso base: estado final alcançado.
busca(Estado, Estado, _, []).

% Passo recursivo: transição válida e evita ciclos.
busca(EstadoAtual, EstadoFinal, Visitados, [(M, C) | Movimentos]) :-
    transicao(EstadoAtual, ProximoEstado, (M, C)),
    \+ member(ProximoEstado, Visitados),
    busca(ProximoEstado, EstadoFinal, [ProximoEstado | Visitados], Movimentos).

% ?- missionarios_e_canibais(Solucao).
% Solução retornará uma lista com os movimentos necessários, por exemplo:
% Solucao = [(1,1), (0,1), (2,0), (1,1), (2,0), (0,1), (1,1)].
