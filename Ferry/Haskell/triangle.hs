-- isTriangle :: [int] -> bool
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c  |  a + b > c = True
                  |  b + c > a = True
                  |  c + a > b = True
                  |  otherwise = False
