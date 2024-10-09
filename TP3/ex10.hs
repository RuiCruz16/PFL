primos :: [Int]
primos = crivo [2..]
  where
    crivo (p:xs) = p : crivo [x | x <- xs, x `mod` p /= 0]

factores :: Int -> [Int]
factores n = fatoresAux n primos
  where
    fatoresAux 1 _ = []
    fatoresAux m (p:ps)
      | m `mod` p == 0 = p : fatoresAux (m `div` p) (p:ps)
      | otherwise      = fatoresAux m ps

a = factores 100