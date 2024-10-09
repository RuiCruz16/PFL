--initt :: [Int] -> [Int]
--initt xs = reverse(tail(reverse xs))

initt :: [Int] -> [Int]
initt xs = take s xs where s = length xs - 1

c = initt [1,2,3,4]