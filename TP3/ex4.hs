insert :: Ord a => a -> [a] -> [a]

insert k [] = [k]

insert k (x:xs) | k <= x = k : x : xs
                | otherwise = x : insert k xs

isort' :: Ord a => [a] -> [a]

isort' xs = foldr insert [] xs

a = isort' [2,3,1,4]