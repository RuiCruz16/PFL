insert :: Ord a => a -> [a] -> [a]

insert k [] = [k]

insert k (x:xs) | k <= x = k : x : xs
                | otherwise = x : insert k xs

isort :: Ord a => [a] -> [a]

isort [] = []

isort (x:xs) = insert x (isort xs)

--b = insert 2 [0,1,3,5,6]

c = isort [4,2,5,1]

