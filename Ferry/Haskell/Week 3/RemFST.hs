remFst :: Int -> [Int] -> [Int]
remFst n [] = []
remFst n (f:fs) 
        |  n == f = fs
        |  otherwise = [f] ++ (remFst n fs)   