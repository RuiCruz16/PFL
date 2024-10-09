algarismosRev :: Int -> [Int]
algarismosRev 0 = []
algarismosRev n = (n `mod` 10) : algarismosRev (n `div` 10)

algarismos :: Int -> [Int]
algarismos n = reverse (algarismosRev n)

a = algarismos 12345
