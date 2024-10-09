metades :: [Int] -> ([Int], [Int])

metades xs = (take l xs, drop l xs) where l = length xs `div` 2

a = metades [1,2,3,4,5,6]