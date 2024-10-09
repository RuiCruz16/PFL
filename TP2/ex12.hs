getDivisors :: Integral a => a -> [a]

getDivisors a = [x | x <- [1.. a `div` 2], a `mod` x == 0]

prime :: Integer -> Bool

prime n = length (getDivisors n) == 1

b = prime 3