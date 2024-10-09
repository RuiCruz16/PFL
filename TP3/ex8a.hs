import Data.Char
palavras :: String -> [String]
palavras [] = []
palavras str =
  let (word, rest) = break isSpace (dropWhile isSpace str)
  in if null word
     then []
     else word : palavras rest

     