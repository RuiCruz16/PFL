--mediana :: (Ord a) => a -> a -> a -> a
{-
mediana x y z
  | (x >= y && x <= z) || (x >= z && x <= y) = x
  | (y >= x && y <= z) || (y >= z && y <= x) = y
  | otherwise                                = z
-}

mediana :: (Ord a, Num a) => a -> a -> a -> a -- Necessário ter os dois 'Ord' e 'Num' para podermos usar operações tanto de comparação como de matemática (+ / -)
mediana x y z = x + y + z - maximum [x, y, z] - minimum [x, y, z]

a = mediana 1 (-5) 2