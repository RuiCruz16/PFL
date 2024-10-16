data Arv a = Empty | Node a (Arv a) (Arv a)

listar :: Arv a -> [a]

listar Empty = []

listar (Node x esq dir) = listar dir ++ [x] ++ listar esq 

a = listar (Node 2 (Node 1 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty)))