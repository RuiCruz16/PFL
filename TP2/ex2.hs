intersperse' :: a -> [a] -> [a]

intersperse' _ [] = []

intersperse' _ [x] = [x]

intersperse' e (x : xs) = x : e : intersperse' e xs

b = intersperse' '-' "banana"

-- 2,3,4,6,7,12,15,20,21,24