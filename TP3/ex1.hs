-- ex 1,2,3,4,7,8,10

mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

a = mapFilter (^2) (>2) [1,2,3,4]