import System.IO (getContents)
import Data.List (words, lines)

main :: IO ()
main = do
    input <- getContents
    let lineCount = length (lines input)
    let wordCount = length (words input)
    let byteCount = length input
    putStrLn (show lineCount ++ " " ++ show wordCount ++ " " ++ show byteCount)
