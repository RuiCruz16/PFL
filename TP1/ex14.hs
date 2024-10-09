curta :: [a] -> Bool

--curta xs = length xs <= 2

curta [] = True -- Listas com 0 elementos -- Se receber uma lista vazia, retorna True
curta [_] = True -- Listas com 1 elemento -- Se receber uma lista com um elemento, seja ele qual for, retorna True
curta [_, _] = True -- Listas com 2 elementos
curta _ = False -- Listas com 3 ou mais elementos

a = curta [1,2]