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
splitSort (x:[])   = [x]
splitSort (x:y:[]) = mergeA [x] [y]  
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
                len <- (getRandomInt size)
                f1 <- genIntList len
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

-- Testable properties: compare sort results between different algorithms,
-- with random input and random length.
-- Time spend 30 min

srt0 = testSorts 10 (\x  -> sort x == (mergeSortF  x)) 
srt1 = testSorts 10 (\x  -> sort x == (mergeSortA  x))
srt2 = testSorts 10 (\x  -> (splitSortA) x == (mergeSortA  x))
srt3 = testSorts 10 (\x  -> (splitSortA) x == (sort  x))

-- Second test sort on known input sets 

-- 1st oddlength sorted
set1 = [1,2,3,4,5,6,7,8,9,10,11,12,13]
rset1 = [1,2,3,4,5,6,7,8,9,10,11,12,13]

-- 2ed even length sorted
set2 = [1,2,3,4,5,6,7,8,9,10,11,12]
rset2 = [1,2,3,4,5,6,7,8,9,10,11,12]

-- 3rd odd length sorted inverted
set3 = [13,12,11,10,9,8,7,6,5,4,3,2,1]
rset3 = [1,2,3,4,5,6,7,8,9,10,11,12,13]

-- 4rd even length sorted inverted
set4 = [12,11,10,9,8,7,6,5,4,3,2,1]
rset4 = [1,2,3,4,5,6,7,8,9,10,11,12]

-- 5ed odd length sorted with split and inverted
set5 = [7,6,5,4,3,2,1,13,12,11,10,9,8]
rset5 = [1,2,3,4,5,6,7,8,9,10,11,12,13]

-- 6th even length sorted with split and inverted
set6 = [7,6,5,4,3,2,1,12,11,10,9,8]
rset6 = [1,2,3,4,5,6,7,8,9,10,11,12]

-- 7th odd length sorted with split and inverted
set7 = [7,8,9,10,11,12,13,1,2,3,4,5,6]
rset7 = [1,2,3,4,5,6,7,8,9,10,11,12,13]

-- 8th even length sorted with split
set8 = [7,8,9,10,11,12,1,2,3,4,5,6]
rset8 = [1,2,3,4,5,6,7,8,9,10,11,12]

-- Running the tests with the not asserted sorter (paranoid not trusting builtin sort)
t1res1 = rset1 == mergeSort set1
t2res1 = rset2 == mergeSort set2
t3res1 = rset3 == mergeSort set3
t4res1 = rset4 == mergeSort set4
t5res1 = rset5 == mergeSort set5
t6res1 = rset6 == mergeSort set6
t7res1 = rset7 == mergeSort set7
t8res1 = rset8 == mergeSort set8

trun1 = t1res1 && t2res1 && t3res1 && t4res1 && t5res1 && t6res1 && t7res1 && t8res1 

-- Test of the F version (after hlint updates)
t1res1f = rset1 == mergeSortF set1
t2res1f = rset2 == mergeSortF set2
t3res1f = rset3 == mergeSortF set3
t4res1f = rset4 == mergeSortF set4
t5res1f = rset5 == mergeSortF set5
t6res1f = rset6 == mergeSortF set6
t7res1f = rset7 == mergeSortF set7
t8res1f = rset8 == mergeSortF set8

trun1f = t1res1f && t2res1f && t3res1f && t4res1f && t5res1f && t6res1f && t7res1f && t8res1f 

t1res2 = rset1 == splitSort set1
t2res2 = rset2 == splitSort set2
t3res2 = rset3 == splitSort set3
t4res2 = rset4 == splitSort set4
t5res2 = rset5 == splitSort set5
t6res2 = rset6 == splitSort set6
t7res2 = rset7 == splitSort set7
t8res2 = rset8 == splitSort set8

trun2 = t1res2 && t2res2 && t3res2 && t4res2 && t5res2 && t6res2 && t7res2 && t8res2 
