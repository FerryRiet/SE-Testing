module Assignment

where 

import Week6
import Lab6
import Control.Monad
import Text.Printf
import Control.Exception
import System.CPUTime
import System.TimeIt


{- Exercise 1:
    Created exM 
-}
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' b e m
    | even e    = multM z z m
    | otherwise = multM b (multM z z m) m
    where z = exM' b (div e 2) m

-- simple test to check the logic
test1 = all test1' [0..10000]
test1' x = (x^33) `mod` 5 == (x^32 `mod` 5) * (x `mod` 5)
test2 = all test2' [0..10000]
test2' x = (x^33) `mod` 5 == (rem ((x^16 `mod` 5) * (x^16 `mod` 5)) 5) * (x `mod` 5)

{-- Measure functions: Results below -}
testNumbersStr = ["m1,m2,m3","m2,m3,m4","m3,m4,m5","m4,m5,m6","m5,m6,m7","m6,m7,m8","m7,m8,m9","m8,m9,m10","m9,m10,m11","m10,m11,m12","m11,m12,m13","m12,m13,m14","m13,m14,m15","m14,m15,m16","m15,m16,m17","m16,m17,m18","m17,m18,m19","m18,m19,m20","m19,m20,m21","m20,m21,m22","m21,m22,m23","m22,m23,m24","m23,m24,m25"]
testNumbers    = [[m1,m2,m3],[m2,m3,m4],[m3,m4,m5],[m4,m5,m6],[m5,m6,m7],[m6,m7,m8],[m7,m8,m9],[m8,m9,m10],[m9,m10,m11],[m10,m11,m12],[m11,m12,m13],[m12,m13,m14],[m13,m14,m15],[m14,m15,m16],[m15,m16,m17],[m16,m17,m18],[m17,m18,m19],[m18,m19,m20],[m19,m20,m21],[m20,m21,m22],[m21,m22,m23],[m22,m23,m24],[m23,m24,m25]]

measureExMs :: IO ()
measureExMs = measureExMs' $ zip testNumbersStr testNumbers

measureExMs' :: [(String,[Integer])] -> IO ()
measureExMs' []     = putStrLn "All done."
measureExMs' (x:xs) = do 
        measureExM (fst x) (snd x)
        measureExMs' xs

-- returns the execution time of the functions
measureExM :: String -> [Integer] -> IO ()
measureExM s (b:e:m:xs) = do
    printf "Test '%s'" s
    test1 <- timeToString  ((exM b e m) `seq` return ())
    putStr (" exM:" ++ test1)
    test2 <- timeToString  ((exM' b e m) `seq` return ())
    putStr (" |exM':" ++ test2)
    if b <= m6 then do
        test3 <- timeToString ((expM b e m) `seq` return ())
        putStrLn (" | expM:" ++ test3)
    else 
        putStrLn " | expM': Skipped."

{- borrowed this function from haskell.org and modified it.
   Link: http://www.haskell.org/haskellwiki/Timing_computations -}
timeToString :: PrintfType b => IO t -> IO b
timeToString a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return $ printf "%0.3f sec" (diff :: Double)

{- Exercise 2
    Result of measureExMs:

        Test 'm1,m2,m3' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm2,m3,m4' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm3,m4,m5' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm4,m5,m6' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm5,m6,m7' exM: 0.000 sec |exM': 0.000 sec |expM: 0.031 sec
        Test 'm6,m7,m8' exM: 0.000 sec |exM': 0.000 sec |expM: 0.281 sec
        Test 'm7,m8,m9' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm8,m9,m10' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm9,m10,m11' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm10,m11,m12' exM: 0.016 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm11,m12,m13' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm12,m13,m14' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm13,m14,m15' exM: 0.016 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm14,m15,m16' exM: 0.031 sec |exM': 0.062 sec | expM': Skipped.
        Test 'm15,m16,m17' exM: 0.062 sec |exM': 0.062 sec | expM': Skipped.
        Test 'm16,m17,m18' exM: 0.109 sec |exM': 0.125 sec | expM': Skipped.
        Test 'm17,m18,m19' exM: 0.234 sec |exM': 0.218 sec | expM': Skipped.
        Test 'm18,m19,m20' exM: 0.359 sec |exM': 0.359 sec | expM': Skipped.
        Test 'm19,m20,m21' exM: 1.014 sec |exM': 1.030 sec | expM': Skipped.
        Test 'm20,m21,m22' exM: 2.324 sec |exM': 2.356 sec | expM': Skipped.
        Test 'm21,m22,m23' exM: 3.791 sec |exM': 3.822 sec | expM': Skipped.
        Test 'm22,m23,m24' exM: 8.096 sec |exM': 8.112 sec | expM': Skipped.
        Test 'm23,m24,m25' exM: 16.723 sec |exM': 16.770 sec | expM': Skipped.
        All done.
        (66.16 secs, 1541471992 bytes)
-}

{- Some random code -}

-- test these numbers
primeList :: Integer -> Integer -> [Integer]
primeList n m = [ x | x <- [n..m], isPrime x]

infix 1 ==> 

(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

-- test all numbers, instead of a random number
prime_test_F' :: Integer -> Bool
prime_test_F' 1 = False
prime_test_F' n = prime_test_F'' (n-1) n
     
prime_test_F'' :: Integer -> Integer -> Bool
prime_test_F'' 0 n = True --error ("test returns true :: " ++ (show n))
prime_test_F'' x n = ((exM x (n-1) n == 1)) && prime_test_F'' (x-1) n

-- test this function for correctness
t1 = all (\x -> prime_test_F' x == isPrime x) [2..1000]

{- Exercise 3 -}

composites :: [Integer]
composites = composites' [4..]
composites' (n:ns) = n : composites' 
   (filter (\ m -> head (factors m) /= m) ns)

{- Exercise 4 -}

-- test using earlier created test function which checks all numbers
testF :: [Integer] -> IO ()
testF [] = print "All done."
testF (k:ks) = do
        isP <- primeF 10 k 
        if isP then do
                print (show k ++ " is a prime!")
                testF ks
        else do
                --print (show k ++ " is a not prime!")
                testF ks
        --where isP = prime_test_F' k

testF1 = testF (take 10000 composites)

{- The first one that may fail is 4, because:
        exM 3 3 4 == 3
        exM 2 3 4 == 0
        exM 1 3 4 == 1  <- fails the primeF test!!!

   Running the test multiple times, the function returns many false positives.
   When upgrading 'the attempts x' in "primeF x y" to 10, there are less false positives.
   For example:
        "1105 is a prime!"
        "6601 is a prime!"
        "8911 is a prime!"
        "10585 is a prime!"
        "All done."
        (533.48 secs, 87417424332 bytes)

        Check with:
        any isPrime [1105,6601,8911,10585]
        False

   - Side note: 
   I used the last line (and commented the first line with isP)
        'where isP = prime_test_F' k'
   to test all possible numbers, and test them to 1. 
   This should be equal to isPrime.
-}

{- Exercise 5 -}
testCarmichael = testF carmichael 

{- Result:
        *Assignment> testF carmichael
        "294409 is a prime!"
        "56052361 is a prime!"
        "118901521 is a prime!"
        "172947529 is a prime!"
        "228842209 is a prime!"
        "1299963601 is a prime!"
        "2301745249 is a prime!"
        "9624742921 is a prime!"
        "11346205609 is a prime!"
        "13079177569 is a prime!"
        "21515221081 is a prime!"
        "27278026129 is a prime!"
        "65700513721 is a prime!"
        "71171308081 is a prime!"
        "100264053529 is a prime!"
        "168003672409 is a prime!"
        "172018713961 is a prime!"
        "173032371289 is a prime!"
        "464052305161 is a prime!"
        "527519713969 is a prime!"
        "663805468801 is a prime!"
        "727993807201 is a prime!"
        "856666552249 is a prime!"
        etc... etc...

        testCarmichael
        "56052361 is a prime!"
        "118901521 is a prime!"
        "172947529 is a prime!"
        "216821881 is a prime!"
        "228842209 is a prime!"
        "1299963601 is a prime!"
        "2301745249 is a prime!"
        "9624742921 is a prime!"
        "11346205609 is a prime!"
        "13079177569 is a prime!"
        etc... slightly different number



        map isPrime carmichael
        [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,Interrupted
        map prime_test_F' carmichael
        [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,Interrupted.
        
        Todo, "Read the entry on Carmichael numbers on Wikipedia to explain what you find." 
-}