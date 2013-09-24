

run :: Integer -> [Integer]
run n   | n < 1     = error "argument is not positive"
        | n == 1    = [1]
        | even n    = n: run (div n 2)
        | odd n     = n: run (3*n+1)