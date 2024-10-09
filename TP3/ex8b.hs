despalavras :: [String] -> String
despalavras [] = ""
despalavras [w] = w
despalavras (w:ws) = w ++ " " ++ despalavras ws