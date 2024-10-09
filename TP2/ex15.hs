import Data.Char

cipher :: Int -> String -> String

cipher n cs = [rotate c | c <- cs]
          where letterToInt c = ord c - ord 'A'
                intToLetter n = chr( ord 'A' + n)
                rotate' c = intToLetter(letterToInt c + n `mod` 26)
                rotate c | isUpper c = rotate' c
                          | otherwise = c

a = cipher 3 "ATAQUE DE MADRUGADA"