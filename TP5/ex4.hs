import Data.Char (toLower)
import Data.List (intersperse)

-- Função do jogo de adivinhação
adivinha :: String -> IO ()
adivinha palavra = jogo palavra (replicate (length palavra) '-') 0

-- Função que implementa a lógica do jogo
jogo :: String -> String -> Int -> IO ()
jogo palavra revelada tentativas = do
    -- Mostra a palavra atual, com letras adivinhadas e traços
    putStrLn revelada
    -- Verifica se o jogador já adivinhou toda a palavra
    if revelada == palavra then
        putStrLn ("Adivinhou em " ++ show tentativas ++ " tentativas!")
    else do
        -- Pergunta ao jogador por uma letra
        putStr "? "
        letra <- getLine
        -- Processa a letra
        let letraLower = toLower (head letra)  -- Usa a letra minúscula para comparação
        let novaRevelada = revelaLetra palavra revelada letraLower
        -- Informa se a letra foi encontrada ou não
        if novaRevelada == revelada then
            putStrLn "Não ocorre"
        else
            return ()
        -- Continua o jogo, incrementando as tentativas
        jogo palavra novaRevelada (tentativas + 1)

-- Função que revela as letras adivinhadas na palavra
revelaLetra :: String -> String -> Char -> String
revelaLetra [] [] _ = []
revelaLetra (p:ps) (r:rs) l
    | toLower p == l = p : revelaLetra ps rs l  -- Se a letra for adivinhada, revela
    | otherwise = r : revelaLetra ps rs l  -- Se não, mantém o traço ou a letra já revelada

-- Função principal que solicita a palavra secreta e inicia o jogo
main :: IO ()
main = do
    -- Pergunta ao primeiro jogador para inserir a palavra secreta
    putStrLn "Digite a palavra secreta: "
    palavra <- getLine
    -- Limpa a tela para que o outro jogador não veja a palavra secreta
    putStrLn "\n\n\n\n\n\n\n\n\n\n"
    -- Inicia o jogo de adivinhação
    adivinha palavra
