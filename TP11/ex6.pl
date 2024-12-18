:-op(550, xf, class).
:-op(560, xfx, of).
:-op(570, xfx, on).
:-op(560, xfx, at).
:-op(550, xfy, :).

% Alínea B)
% IMPORTANTE! No exercício anterior havia casos de erro pq os opradores tinham todos a mesma prioridade, com prioridades diferentes não precisamos de aplicar as regras de associatividade. 

% tp class of pfl on mondays at 10:30.

                      on
                     /  \
                   of    \
                  /   \   \
                 /    pfl  at
                /         /   \
              class      /     \ 
              /       mondays   :
             tp                / \
                              10  30
