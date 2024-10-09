max3, min3 :: Ord a => a -> a -> a -> a

{-
max3 a b c | a > b && a > c = a
           | b > a && b > c = b
           | otherwise = c
-}

max3 a b c = max (max a b) c 

{-
min3 a b c | a < b && a < c = a
           | b < a && b < c = b
           | otherwise = c
-}

min3 a b c = min (min a b) c 

d = max3 1 2 3

e = min3 4 65 7