module Lab5 

where

import Week5
import Data.List


{-
assert1 :: (a -> b -> Bool) -> (a -> b) -> a -> b 
assert1 p f x = if p x (f x) then f x 
                else error "assert1"
-}


mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA  = assert1 (\ s x ->  x == sort s ) mergeSrt 


split :: [a] -> ([a],[a])
split xs = let
            n = (length xs) `div` 2
            in
                (take n xs, drop n xs)

splitSort :: Ord a => [a] -> [a]
splitSort [] = [] 
splitSort [x] = [x]
splitSort (x:y:[]) = merge [x] [y]  
splitSort x =  merge (splitSort s)  (splitSort y)
               where (s,y) = split x 