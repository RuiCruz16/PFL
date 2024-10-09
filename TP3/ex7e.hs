elem' :: Eq a => a -> [a] -> Bool
elem' x xs = any (== x) xs

a = elem' 2 [1,2,3]