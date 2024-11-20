type Species = (String, Int)

type Zoo = [Species]

data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram

isEndangered :: Species -> Bool

isEndangered s
              |  (head aux <= 100) = True
              | otherwise = False
              where aux = [y | (x,y) <- [s]]

updateSpecies :: Species -> Int -> Species

updateSpecies s newbabies = head aux
                            where aux = [(x, y + newbabies) | (x,y) <- [s]]

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (x:xs) f
                    | f x = x : filterSpecies xs f
                    | otherwise = filterSpecies xs f

countAnimals :: Zoo -> Int
countAnimals zoo = foldl (\n x -> x + n) 0 aux
                  where aux = [populations | (_, populations) <- zoo]

substring :: (Integral a) => String -> a -> a -> String
substring string inicio fim = [x | idx <- [inicio..fim], (x,y) <- aux, idx == y]
                            where aux = zip string [0..]

hasSubstr :: String -> String -> Bool
hasSubstr [] _ = False
hasSubstr s1 s2
                | substring s1 0 n == s2 = True
                | otherwise = hasSubstr (tail s1) s2
                where n = length s2 - 1

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr zoo string = (aux1, aux2)
                            where aux1 = [(name, count) | (name,count) <- zoo, hasSubstr name string]
                                  aux2 = [(name, count) | (name,count) <- zoo, not (hasSubstr name string)]

rabbits :: (Integral a) => [a]
rabbits = rabbitsAux [2,3]

rabbitsAux :: (Integral a) => [a] -> [a]
rabbitsAux (x:y:xs) = x : rabbitsAux (y : (x + y) : xs)

rabbitYears :: (Integral a) => a -> Int
rabbitYears year = length (takeWhile (< year) rabbits)

calculateLeft :: Dendrogram -> Int
calculateLeft (Leaf _) = 0
calculateLeft (Node left x _) = x + calculateLeft left

calculateRight :: Dendrogram -> Int
calculateRight (Leaf _) = 0
calculateRight (Node _ x right) = x + calculateLeft right

dendroWidth :: Dendrogram -> Int
dendroWidth a = calculateLeft a + calculateRight a

dendroInBounds :: Dendrogram -> Int -> [String]
dendroInBounds (Leaf str) val = if val >= 0 then [str] else []
dendroInBounds (Node left x right) val = dendroInBounds left (val - x) ++ dendroInBounds right (val - x) 

myDendro :: Dendrogram 
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

test1 :: Species
test1 = ("GONCALO", 101)

test2 :: Species
test2 = ("MIGUEL", 7)

test3 :: Species
test3 = ("BIA", 3)

test4 :: Zoo
test4 = [test1, test2, test3]

b = filterSpecies test4 isEndangered

a = updateSpecies test1 2

c = countAnimals test4

d = substring "TESTEOLA" 0 3

e = hasSubstr "OLAGONCALO" "GONCALOO"

f = sortSpeciesWithSubstr test4 "BIA"

g = rabbits