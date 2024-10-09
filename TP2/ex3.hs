mdc :: Integer -> Integer -> Integer

mdc a 0 = a

mdc a b = mdc b (a `mod` b)

b = mdc 4 2