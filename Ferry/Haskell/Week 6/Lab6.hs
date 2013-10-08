module Lab where

import Data.List
import System.Random
import System.TimeIt
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]


{-
-- Time spend: After reading Week6.hs +- 30 min

-- X^31 = X ( X^15 . X^15 ) = X ( X ( X^7 . X^7 ) . X ( X^7 . X^7 )) ........   
-}
expMy :: Integer -> Integer -> Integer -> Integer
expMy _ 0 _ = 1     
expMy x y n | even y = multM z z n 
            | otherwise = multM x (multM z z n) n
              where  z  = expMy x (y `div` 2) n

--    Compare performance test

-- Modified test framework from Week 2 code
-- Time spend 30 minutes
-- timeIt ( do print( show (expM (m7*100) m8 m7) ))


test :: (Integer -> Integer -> Integer -> Integer) -> [(Integer,Integer,Integer)] -> IO ()
test _ [] = print "Tests finished"
test p ((x,y,w):fs) =  do  
                        timeIt ( putStr ( "X=" ++ show x ++ " Y=" ++  show y ++ " N=" ++ show w ++ " " ++ "(X^Y mod N) =" ++ show (p x y w) ++ " ")) 
                        test p fs 

testVminimal = [(m1,m2,m3),(m3,m4,m5),(m5,m6,m7),(m6,m7,m8)]

testVfull = [(m1,m2,m3),(m3,m4,m5),(m5,m6,m7),(m6,m7,m8),(m7,m8,m9),(m9,m10,m11),(m11,m12,m13),(m13,m14,m14)]

{-- Test results 
    Test run :

    test expMy testVFull

    X=3 Y=7 N=31 (X^Y mod N) =17 CPU time:   0.00s
    X=31 Y=127 N=8191 (X^Y mod N) =1217 CPU time:   0.00s   
    X=8191 Y=131071 N=524287 (X^Y mod N) =258260 CPU time:   0.00s
    X=131071 Y=524287 N=2147483647 (X^Y mod N) =827082527 CPU time:   0.00s
    X=524287 Y=2147483647 N=25843009213693951 (X^Y mod N) =727939296055450483 CPU time:   0.00s
    X=2305843009213693951 Y=61819642690137449562111 N=162259276828010288127 (X^Y mod N) =144143242456290764409 CPU time:   0.00s
    X=162259276829213578010288127 Y=17014116046715884105727 N=68647976674028291115057151 (X^Y mod N) =642710669350367024955752 CPU time:   0.01s
    "Tests finished"

    (Note : For readability the numbers are truncated)

    test expM testVFull

    X=3 Y=7 N=31 (X^Y mod N) =17 CPU time:   0.00s
    X=31 Y=127 N=8191 (X^Y mod N) =1217 CPU time:   0.00s
    X=8191 Y=131071 N=524287 (X^Y mod N) =258260 CPU time:   0.10s
    X=131071 Y=524287 N=2147483647 (X^Y mod N) =827082527 CPU time:   0.78s
    X=524287 Y=2147483647 N=2305843009213693951 (X^Y mod N) =^C

    Will probably not terminate during this course.
-}


{-
  After the hint in the assignment 2, I wrote composites 
  based on the sieve vwhich really is a very bad solution. 
  (but works).

--primes = sieve [2..]
--sieve (n:ns) = n : sieve (filter (\ m -> rem m n /= 0) ns)


  After a rethink I came op with c2

  compared:

  timeIt (print (show (take 10000 composites)))
  ...
  CPU time: 1097.97s

  timeIt (print (show (take 10000 c2)))
  ...
  CPU time:   0.20s

-}
composites :: [Integer]
composites = composites' [4..]
composites' (n:ns) = n : composites' (filter (\ m -> length (take 2 (factors m)) == 2) ns)

c2 = [ i | i <- [4..], head (factors i) /= i ]

testF :: [Integer] -> IO ()
testF [] = print "Test done!"
testF (x:xs) = do
            result <- prime_test_F x
            if result 
            then
                do print ("Failed composite " ++ show x)
                   testF xs
            else
                testF xs 
