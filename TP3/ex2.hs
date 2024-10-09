dec2int :: [Int] -> Int

dec2int = foldl update 0
          where update acc d = 10 * acc + d

a = dec2int [1,2,3,4]