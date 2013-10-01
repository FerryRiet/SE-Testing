module Assignment

where

import Data.List
import Week4 
import Week5
import RandomSudoku

{- Precondition -}
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)
{- Postcondition: list is empty or sorted -}