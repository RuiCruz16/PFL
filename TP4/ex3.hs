data Arv a = Empty | Node a (Arv a) (Arv a)

nivel :: Int -> Arv a -> [a]

nivel _ Empty = []

nivel 0 (Node x esq dir) = [x]

nivel n (Node x esq dir) = nivel (n-1) esq ++ nivel (n-1) dir

a = nivel 2 (Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 Empty Empty))