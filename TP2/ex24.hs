merge :: Ord a => [a] -> [a] -> [a]

merge [] ys = ys

merge xs [] = xs

merge (x:xs) (y:ys) | x <= y = x : merge  xs ( y: ys)
                    | otherwise = y : merge (x : xs) ys

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]

msort [] = []

msort [x] = [x]

msort xs = merge (msort lh) (msort rh)
    where (lh, rh) = halves xs


a = msort [1,4,5,3,2]