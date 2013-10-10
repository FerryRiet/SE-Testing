module LabAssign6
where

import Week6
import Data.List
import System.Random

{-
    Assignment: 1 & 2

    if b is even and b > 0  -->     (a2)(b/2)
    if b is odd             -->     a^b = a*(a^2)^((b-1)/2)
    if b = 0                -->     1

    Takes time: O( log(b) )
-}
exM' :: Integer -> Integer -> Integer -> Integer
exM' a b c = rem (ex a b) c

ex :: Integer -> Integer -> Integer
ex a 0 = 1
ex a b = if (rem b 2 == 0) then
            (a^2)^ quot b 2
         else
            ((a^2) ^ quot (b-1) 2) * a

-- Test if results of own implementation are valid
-- Assuming results from expM are correct!
testValid = and [ (exM' a b c)  == (expM a b c) | a <- [2,4..100], b <- [1..50], c <- [1..75]]

-- ToDo: test if impl is more efficient than expM

{--
    Assignment: 3
--}
composites :: [Integer]
composites = composites' [4..]

composites' :: [Integer] -> [Integer]
composites' xs = [ m | m <- xs, head  (factors m)  /= m ]

{--
    Assignment: 4
--}
testF :: Int -> [Integer] -> IO ()
testF _ [] = return ()
testF k (x:xs) = do
            test <- primeF k x
            if test then
                do
                    print (show x ++ " is a prime")
                    testF k xs
            else
                testF k xs

t1 = testF 1 $ take 1000 composites
t2 = testF 2 $ take 1000 composites
t3 = testF 3 $ take 1000 composites
t4 = testF 4 $ take 1000 composites

{--
    Assignment: 5
--}
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      isPrime (6*k+1),
      isPrime (12*k+1),
      isPrime (18*k+1) ]

t1' = testF 1 $ take 1000 carmichael
t2' = testF 2 $ take 1000 carmichael
t3' = testF 3 $ take 1000 carmichael
t4' = testF 4 $ take 1000 carmichael

{--
    Assignment: 6
--}
testMR :: Int -> [Integer] -> IO ()
testMR _ [] = return ()
testMR k (x:xs) = do
            test <- primeMR k x
            if test then
                do
                    print (show x ++ " is a prime")
                    testMR k xs
            else
                testMR k xs

mrT1 = testMR 1 $ take 1000 carmichael
mrT2 = testMR 2 $ take 1000 carmichael
mrT3 = testMR 3 $ take 1000 carmichael
mrT4 = testMR 4 $ take 1000 carmichael

{--
    Assignment: 7
--}
mersenneTest :: Int -> [Integer] -> IO ()
mersenneTest k [] = return ()
mersenneTest k (x:xs) =
                        do
                            t <- primeMR k x
                            if t then
                                mersenneTest k xs
                            else
                                do
                                    print (show x ++ " is not a mersenne prime")
                                    mersenneTest k xs

mp :: [Integer]
-- The numbers came from Wikipedia
mp = [57885161, 43112609, 42643801, 2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 1279]

testM = mersenneTest 1 mp

