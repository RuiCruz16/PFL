maxpos :: [Int] -> Int
maxpos [] = 0
maxpos (x:xs)
              | x > next = x
              | otherwise = next
              where next = maxpos xs

dups :: [a] -> [a]
dups xs = dupsAux xs True

dupsAux :: [a] -> Bool -> [a]
dupsAux [] _ = []
dupsAux (x:xs) r
                | r = x : x : dupsAux xs False
                | otherwise = x : dupsAux xs True

transforma :: String -> String
transforma [] = []
transforma (x:xs)
                  | x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' = x : 'p' : x : transforma xs
                  | otherwise = x : transforma xs

type Vector = [Int]
type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta [] = []
transposta m = [ head x | x <- m] : transposta [tail x | x <- m, tail x /= []]

prodInterno :: Vector -> Vector -> Int
prodInterno [] _ = 0
prodInterno _ [] = 0

prodInterno (x:xs) (y:ys) = x * y + prodInterno xs ys

a = prodInterno [1,2,3] [4,3,2]

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = prodMatAux m1 (transposta m2)

prodMatAux :: Matriz -> Matriz -> Matriz
prodMatAux [] [] = []
prodMatAux m1 m2 = [[prodInterno v1 v2 | v2 <- m2] | v1 <- m1]

data Arv a = F | N a (Arv a) (Arv a) deriving (Show)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N a esq dir) = N (alturasAux (N a esq dir)) (alturas esq) (alturas dir)

alturasAux :: Arv a -> Int
alturasAux F = 0
alturasAux (N a esq dir) = 1 + max (alturasAux esq) (alturasAux dir)

equilibrada :: Arv a -> Bool 
equilibrada F = True
equilibrada (N a a1 a2) = equilibradaAux (alturas (N a a1 a2))

equilibradaAux :: Arv Int -> Bool
equilibradaAux F = True
equilibradaAux (N a F F) = True
equilibradaAux (N a F (N b b1 b2)) = b == 1
equilibradaAux (N a (N b b1 b2) F) = b == 1
equilibradaAux (N a (N b b1 b2) (N c c1 c2)) = b == c && equilibradaAux (N b b1 b2) && equilibradaAux (N c c1 c2)

f :: (a -> b -> c) -> b -> a -> c
f aux b a = aux a b
