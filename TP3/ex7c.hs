reverse' :: [a] -> [a]

reverse' = foldr (\x acc -> acc ++ [x]) []
a = reverse' [1,2,3,4]