module Assignment

where

import Data.List
import Week4 
import Week5
import RandomSudoku

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

{- The output is a permutation of the input -}
permutationProp :: Ord a => [a] -> [a] -> Bool
permutationProp xs ys = isPermutation xs ys

{- Length of input and output of the list is the same -}
lengthProp :: Ord a => [a] -> [a] -> Bool
lengthProp xs ys = length xs == length ys

{- The output list is the sorted version of the input -}
sortedOutputProp :: Ord a => [a] -> [a] -> Bool
sortedOutputProp xs ys = sort xs == ys

-- isPermutation compares two lists and returns True if the list is a permutation
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []             = True
isPermutation [] _              = False
isPermutation _ []              = False
isPermutation (x:xs) (y:ys)  
        | x == y = isPermutation xs ys
        | otherwise = isPermutation xs (delete x (y:ys))

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 permutationProp 
          $ assert1 sortedOutputProp 
            mergeSrt

-- indication of time spent: about 30 minutes.

mergeSrtSplit :: Ord a => [a] -> [a]
mergeSrtSplit []  = []
mergeSrtSplit [x] = [x]
mergeSrtSplit xs  = merge (mergeSrtSplit (fst ys)) (mergeSrtSplit (snd ys))
        where ys = split xs

split :: [a] -> ([a],[a])
split xs = let 
             n = (length xs) `div` 2 
           in 
             (take n xs, drop n xs)

sublistProp1 :: Eq a => [a] -> [a] -> Bool
sublistProp1 xs ys = 
  sublist xs ys

mergeSrtSplitA :: Ord a => [a] -> [a]
mergeSrtSplitA = assert1 permutationProp 
               $ assert1 sortedOutputProp 
               $ assert1 sublistProp1
                 mergeSrtSplit

-- indication of time spent: about 30 minutes.

