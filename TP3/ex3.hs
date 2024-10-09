zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith' _ [] _ = []

zipWith' _ _ [] = []

zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

a = zipWith' (+) [1,2] [4,5,6]