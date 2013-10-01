module AssignWeek5
where
import Data.List
import Week5

{-
    Assignment: 1
    Assertion: output of the function should be sorted correctly.
-}

-- From the course notes
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)
-- The assertion impl.
aMergeSrt :: Ord a => [a] -> [a]
aMergeSrt = assert1 (\ x xs -> sorted xs ) mergeSrt

{-
    Assignment: 2
    Assertion: same as assignment 1
-}
split :: [a] -> ([a],[a])
split xs = let
                n = (length xs) `div` 2
           in
                (take n xs, drop n xs)

mergeSrt' :: Ord a => [a] -> [a]
mergeSrt' [] = []
mergeSrt' [x] = [x]
mergeSrt' xs = let
                    first = fst (split xs)
                    second = snd (split xs)
               in
                    merge (mergeSrt' first) (mergeSrt' second)
-- The assertion impl.
aMergeSrt' :: Ord a => [a] -> [a]
aMergeSrt' = assert1 (\ x xs -> sorted xs ) mergeSrt'

