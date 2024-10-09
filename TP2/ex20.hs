import Data.ByteString (tails)
transpose :: [[a]] -> [[a]]

transpose [] = []

transpose ([] : _) = []

transpose xss = heads : transpose tails
                      where heads = [h | (h:_) <- xss]
                            tails = [t | (_:t) <- xss]


a = transpose [[0,1,2], [3,4,5]]