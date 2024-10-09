binom' :: Integer -> Integer -> Integer
binom' n k | k < n - k = (product [(n - k + 1)..n]) `div` (product [1..k])
            | otherwise = (product [(k+1)..n]) `div` (product [1..(n-k)])

a = binom' 10 2