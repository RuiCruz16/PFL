import Data.Char

-- Função para cifrar uma string com um número de posições de rotação
cipher :: Int -> String -> String
cipher n cs = [rotate c | c <- cs]
    where 
        -- Função para rotacionar uma letra maiúscula
        rotateUpper c = chr (ord 'A' + (ord c - ord 'A' + n) `mod` 26) -- (ord c - ord 'A') retorna o index do caracter -- + n é a rotação -- mod 26 para garantir que o index não ultrapasse o alfabeto -- somamos tudo isso a ord 'A' para obter o index da letra cifrada
        -- Função para rotacionar uma letra minúscula
        rotateLower c = chr (ord 'a' + (ord c - ord 'a' + n) `mod` 26) -- mesmo caso do rotateUpper só que para letras minúsculas
        -- Aplica a rotação somente se for letra
        rotate c
            | isUpper c = rotateUpper c
            | isLower c = rotateLower c
            | otherwise = c  -- Não modifica caracteres não alfabéticos

-- Função principal: cifra com ROT13 (13 posições)
-- Lê a entrada-padrão, aplica a cifra e imprime o resultado
-- Função principal do exercício 3
main :: IO ()
main = do
    input <- getContents  -- Lê a entrada-padrão / Coloca o conteúdo da entrada-padrão na variável input
    putStrLn (cipher 13 input)  -- Aplica a cifra de César com rotação de 13
