import System.IO (isEOF)

main :: IO ()
main = do
    done <- isEOF
    if done
        then return ()
        else do
            line <- getLine -- getLine espera até que o utilizador digite uma linha de texto e pressione Enter
            putStrLn (reverse line) -- putStrLn coloca a string na saída-padrão (normalmente o terminal), adicionando uma nova linha no final
            main