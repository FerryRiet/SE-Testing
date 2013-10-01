module Assignment

where

import Data.List
import Week4 
import Week5
import RandomSudoku

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

{- Length of input and output of the list is the same -}
lengthProp :: Ord a => [a] -> [a] -> Bool
lengthProp xs ys = length xs == length ys

{- The output list is the sorted version of the input -}
sortedOutputProp :: Ord a => [a] -> [a] -> Bool
sortedOutputProp xs ys = sort xs == ys

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 lengthProp $ assert1 sortedOutputProp mergeSrt

-- indication of time spent: about 30 minutes.

