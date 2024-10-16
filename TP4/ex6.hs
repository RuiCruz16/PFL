data Arv a = Empty | Node a (Arv a) (Arv a) deriving Show

rightMost :: Arv a -> a

rightMost (Node x _ Empty) = x

rightMost (Node _ _ dir) = rightMost dir

b = rightMost (Node 2 (Node 1 Empty Empty) (Node 3 (Node 4 Empty Empty) (Node 5 Empty Empty)))

remove :: Ord a => a -> Arv a -> Arv a
remove _ Empty = Empty

remove x (Node y esq dir)
    | x < y = Node y (remove x esq) dir
    | x > y = Node y esq (remove x dir) 
    | otherwise = case esq of
        Empty -> dir  
        _ -> case dir of
            Empty -> esq  
            _ -> let maiorEsq = rightMost esq
                 in Node maiorEsq (remove maiorEsq esq) dir

a = remove 2 (Node 1 (Node 2 Empty Empty) Empty)