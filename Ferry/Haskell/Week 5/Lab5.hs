{-
   Lab assignment 5
   Author Ferry (2013-9-30)
-}

module Lab5 

where

import Week5
import Data.List
import System.Random

{-
assert1 :: (a -> b -> Bool) -> (a -> b) -> a -> b 
assert1 p f x = if p x (f x) then f x 
                else error "assert1"
-}

-- MergeSort
-- Time spend 15 minutes

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:xs) = mergeA [x] (mergeSort xs)


-- The next implementation by running hlint

mergeSortF :: Ord a => [a] -> [a]
mergeSortF = foldr (\ x -> mergeA [x]) [] 

-- Next the asserted one

mergeSortA :: Ord a => [a] -> [a]
mergeSortA  = assert1 (\ s x ->  x == sort s ) mergeSort 


-- Split sort 
-- Time spend 30 minutes

split :: [a] -> ([a],[a])
split xs = let
            n = (length xs) `div` 2
            in
                (take n xs, drop n xs)

splitSort :: Ord a => [a] -> [a]
splitSort [] = [] 
splitSort (x:y:[]) = merge [x] [y]  
splitSort x =  mergeA (splitSort s)  (splitSort y)
               where (s,y) = Lab5.split x 

splitSortA :: Ord a => [a] -> [a]
splitSortA  = assert1 (\ s x ->  x == mergeSortA s ) splitSort 


{-
    Generate random lists for sorting
    Time spend 15 minutes
-}

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: Int -> IO [Int]
genIntList n = genIntList' n 

genIntList'  :: Int -> IO [Int]
genIntList'  0 =  return [] 
genIntList'  n =  do c <- getRandomInt 16
                     l <- genIntList' (n-1)
                     return (c:l)

genIntLists :: Int -> Int -> IO [[Int]]
genIntLists 0 _ = return []
genIntLists n size = do
                f1 <- genIntList size
                f2 <- genIntLists (n-1) size
                return (f1:f2)

-- Modified test framework from Week 2 code
-- Time spend 10 minutes

test :: Int -> ([Int] -> Bool) -> [[Int]] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n p (f:fs) = 
  if (p f)
  then do print ("pass on:" ++ show f)
          test n p fs
  else error ("failed test on:" ++ show f)

testSorts :: Int -> ([Int] -> Bool) -> IO ()
testSorts n prop = do 
  fs1 <- genIntLists n 100
  test n prop fs1

-- Testable properties: compare sort results between different algorithms
-- with random input.
-- Time spend 30 min

srt0 = testSorts 10 (\x  -> sort x == (mergeSortF  x))
srt1 = testSorts 10 (\x  -> sort x == (mergeSortA  x))
srt2 = testSorts 10 (\x  -> (splitSortA) x == (mergeSortA  x))
srt3 = testSorts 10 (\x  -> (splitSortA) x == (sort  x))

-- Test sort on known input sets

-- 1st not even length sorted
set1 = [1,2,3,4,5,6,7,8,9,10,11,12,13]
rset1 = [1,2,3,4,5,6,7,8,9,10,11,12,13]

-- 2ed even length sorted
set2 = [1,2,3,4,5,6,7,8,9,10,11,12]
rset2 = [1,2,3,4,5,6,7,8,9,10,11,12]

-- 3rd not even length sorted inverted
set3 = [13,12,11,10,9,8,7,6,5,4,3,2,1]
rset3 = [1,2,3,4,5,6,7,8,9,10,11,12,13]

-- 4rd even length sorted inverted
set4 = [12,11,10,9,8,7,6,5,4,3,2,1]
rset4 = [1,2,3,4,5,6,7,8,9,10,11,12]

-- 5ed even length sorted
set5 = [13,12,11,10,9,8,7,6,5,4,3,2,1]
rset2 = [1,2,3,4,5,6,7,8,9,10,11,12,13]



