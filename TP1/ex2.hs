areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
                      where s = (a + b + c) / 2

d = areaTriangulo 1 2 3