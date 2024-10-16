data Arv a = Empty | Node a (Arv a) (Arv a)

sumArv :: Num a => Arv a -> a

sumArv Empty = 0
sumArv (Node x esq dir) = x + sumArv esq + sumArv dir

a = sumArv (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))