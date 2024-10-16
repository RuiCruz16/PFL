data Arv a = Empty | Node a (Arv a) (Arv a) deriving Show -- Show para visualizar a árvore

mapTree :: (a -> b) -> Arv a -> Arv b

mapTree _ Empty = Empty

mapTree f (Node x esq dir) = Node (f x) (mapTree f esq) (mapTree f dir)

a = mapTree (^2) (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))


