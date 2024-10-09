reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

a = reverse' [1,2,3,4]