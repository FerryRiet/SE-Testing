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

{-Assignment 4

      First Single Fermat test gave 4 as a minimal prime (failed composite) 

      Test runs:

      testF 1 $ take 10000 c2
      
      Large set of failures with 4 as a minimum

      testF 2 $ take 10000 c2

      An average of 8 failures with 9 as a minimum.

      testF 3 $ take 10000 c2

      An average of 5 failures with 15 as a minimum.

      testF 4 $ take 10000 c2

      An average of 3 failures with 561 as a minimum.

      testF 5 $ take 10000 c2

      An average of 2 failures with 91 as a minimum.

      testF 6 $ take 10000 c2

      An average of 1 failures with 1105 as a minimum.

      Finally with the increase of k the number of tests the composite has to pass
      make it more likely to detect it is a composite. A prime will pass all tests 
      the behavior of composites is not described.


-}
testF :: Int -> [Integer] -> IO ()
testF n [] = print "Test done!"
testF n (x:xs) = do
            result <- primeF n x
            if result 
            then
                do print ("Failed composite it's a prime :" ++ show x)
                   testF n xs
            else
                testF n xs 

{- Assignment 5
    Testing the Fermat's primality check for multiple k on Carmichael numbers.

    testF 1 $ take 1000 carmichael
    


    testF 2 $ take 1000 carmichael
    testF 4 $ take 1000 carmichael
    testF 8 $ take 1000 carmichael
    testF 16 $ take 1000 carmichael
    testF 32 $ take 1000 carmichael


-}
testMR :: Int -> [Integer] -> IO ()
testMR n [] = print "Test primeF done!"
testMR n (x:xs) = do
            result <- primeMR n x
            if result 
            then
                do print ("Failed composite it's a prime :" ++ show x)
                   testMR n xs
            else
                testMR n xs 

{-
Assignment 8

You can use the Miller-Rabin primality check to discover some large Mersenne
primes. The recipe: take a large prime p, and use the Miller-Rabin algorithm
to check whether 2p 􀀀 1 is also prime. Find information about Mersenne primes
on internet and check whether the numbers that you found are genuine Mersenne
primes. Report on your ndings.
-}

mersenne :: Integer -> IO ()
mersenne n = do
              --p <- n -- 
              res <- primeMR 4 (2^n - 1)  
              nextP <- returnNextPrime n
              if res
                then
                  do print ("Mersenne " ++ show n)
                     mersenne (nextP)
                else
                  mersenne (nextP)






{-
  Assignment 8 

  Step one ;

  Generate two random primes


-}

getRandomprime :: IO Integer
getRandomprime = do
                    rn <- randomRIO (2^127, 2^129)  :: IO Integer
                    returnNextPrime rn

returnNextPrime :: Integer -> IO Integer
returnNextPrime n = do 
                     result <-  primeMR 4 (n+1)
                     if result
                     then
                       return (n+1) 
                     else
                       returnNextPrime (n+1)



