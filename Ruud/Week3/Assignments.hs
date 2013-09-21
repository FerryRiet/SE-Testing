module Assigments

where 

import Week2
import Data.List
import Data.Char
import System.Random
import Week3
import Techniques

-- genIntList creates a list of 'n' Ints from zero to 'm'
genIntList :: IO [Int]
genIntList = do 
        n <- getRandomInt 5
        m <- getRandomInt 5
        getRandomInts n m 
                   
-- getRandomInts creates a list of 'n' Ints from zero to 'm'
getRandomInts :: Int -> Int -> IO [Int]
getRandomInts 0 m = return []
getRandomInts n m = do
        f <- getRandomInt m
        fs <- getRandomInts (n-1) m
        return (f:fs)

-- a permutation is of the same length
-- a permutation contains the same numbers but may appear in different order, so the ordered lists of the items are the same
-- the sum of a permutation is the same
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []             = True
isPermutation [] _              = False
isPermutation _ []              = False
isPermutation (x:xs) (y:ys)  
        | x == y = isPermutation xs ys
        | otherwise = isPermutation xs (delete x (y:ys))
