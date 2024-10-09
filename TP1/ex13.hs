safetail :: [a] -> [a]
--safetail xs = if null xs then [] else tail xs

{-
safetail xs
  | null xs   = []
  | otherwise = tail xs
-}

safetail [] = []
safetail (_:xs) = xs -- O primeiro elemento da lista não importa (_), serve apenas para verificar que existe pelo menos um elemento, mesmo retornando []

a = safetail [1]