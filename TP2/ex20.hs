import Data.ByteString (tails)
transpose :: [[a]] -> [[a]]

transpose [] = []

transpose ([] : _) = []

transpose xss = heads : transpose tails
                      where heads = [h | (h:_) <- xss]
                            tails = [t | (_:t) <- xss]


a = transpose [[0,1,6], [2,3,7], [4,5,8]]

--0 1 6
--2 3 7
--4 5 8