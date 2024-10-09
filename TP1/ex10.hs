calculateIMC :: Fractional a => a -> a -> a
calculateIMC p a = p / (a^2)

classifica :: Float -> Float -> String
classifica p a | calculateIMC p a < 18.5 = "baixo peso"
               | calculateIMC p a >= 18.5 && calculateIMC p a < 25 = "peso normal"
               | calculateIMC p a >= 25 && calculateIMC p a < 30 = "excesso de peso"
               | calculateIMC p a >= 30 = "obesidade"


teste = classifica 70 1.7