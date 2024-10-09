--lastt :: [Int] -> Int
--lastt xs = head(reverse xs)

lastt :: [Int] -> Int
lastt xs = head(drop l xs) where l = length xs - 1

b = lastt [1,2,3,4,5]
