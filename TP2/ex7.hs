approx :: Int -> Double

-- approx a = 4 * sum [(-1) ^ n / (2 * fromIntegral n + 1) | n <- [0..a]] 

approx n = 4 * sum [factor k | k <- [0..n]] where factor k = (-1)^k / fromIntegral (2 * k + 1)

approx' :: Int -> Double

-- approx' a = 12 * sum [(-1)^n / (fromIntegral n +1) ** 2 | n <- [0..a]]

approx' n = sqrt (12 * sum [factor k |k <- [0..n]]) where factor k = (-1)^k / fromIntegral ((k+1)^2) -- sqrt para obtermos pi ao invés de pi^2

a = approx' 10