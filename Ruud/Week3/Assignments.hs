module Assigments

where 

import Week2
import Data.List
import Data.Char
import System.Random
import Week3
import Techniques

-- genIntList creates a list of zero to ten random Ints
genIntList :: IO [Int]
genIntList = do d <- getRandomInt 10 ;
                getRandomInts 2
                   
getRandomInts :: Int -> IO [Int]
getRandomInts 0 = return []
getRandomInts n = do
        f <- getRandomInt 10
        fs <- getRandomInts (n-1)
        return (f:fs)

-- a permutation is of the same length
-- a permutation contains more then one number
-- a permutation contains the same numbers 
-- in a permutation the numbers occur in differnt order
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] _              = False
isPermutation [x] _             = False
isPermutation _ []              = False
isPermutation _ [y]             = False
isPermutation (xs) (ys)         = 
        if length xs == length ys && xs /= ys && compareLists xs ys
        then True 
        else False

compareLists :: Eq a => [a] -> [a] -> Bool
compareLists [] []              = True
compareLists [] _               = False
compareLists (x:xs) (ys)        = compareLists xs (removeItem x ys) 

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = ys
                    | otherwise = y : removeItem x ys
