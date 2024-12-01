% Não é suposto usar as libraries nestes exercícios a não ser que permitam o usa de alguma(s) em específico!

% EXERCÍCIOS RECOMENDADOS: 1, 2[bde], 3[gij], 4[bdf], 5[af], 7

% A) | ?- [a | [b, c, d] ] = [a, b, c, d] --> true

% B) | ?- [a | b, c, d ] = [a, b, c, d] --> false

% C) | ?- [a | [b | [c, d] ] ] = [a, b, c, d] --> true

% D) | ?- [H|T] = [pfl, lbaw, fsi, ipc] --> true

% E) | ?- [H|T] = [lbaw, ltw] --> true

% F) | ?- [H|T] = [leic] --> true

% G) | ?- [H|T] = [] --> false

% H) | ?- [H|T] = [leic, [pfl, ipc, lbaw, fsi] ] --> true

% I) | ?- [H|T] = [leic, Two] --> true // Two neste caso é uma variável (por T ser maiúscula) logo pode representar qualquer valor (p.ex. 5)

% J) | ?- [Inst, feup] = [gram, LEIC] --> false // Explicação: feup != LEIC, no caso de Inst estamos a colocar o valor `gram` na variável Inst

% K) | ?- [One, Two | Tail] = [1, 2, 3, 4] --> true

% L) | ?- [One, Two | Tail] = [leic | Rest] --> false
