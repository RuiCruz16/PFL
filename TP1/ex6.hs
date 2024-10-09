
raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ((-b + sqrt (aux)) / (2 * a), (-b - sqrt (aux)) / (2 * a))
              where aux = b^2 - 4 * a * c

d = raizes 1 (-5) 6