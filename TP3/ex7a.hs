add' :: [a] -> [a] -> [a]

add' xs ys = foldr (:) ys xs