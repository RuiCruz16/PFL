testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c  | a  < b + c = True
                      | b  < a + c = True
                      | c  < a + b = True
                      | otherwise = False


d = testaTriangulo 1 2 3