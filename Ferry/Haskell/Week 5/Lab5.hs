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


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:xs) = mergeA [x] (mergeSort xs)

mergeSortA :: Ord a => [a] -> [a]
mergeSortA  = assert1 (\ s x ->  x == sort s ) mergeSort 


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

genIntLists :: Int -> IO [[Int]]
genIntLists 0 = return []
genIntLists n = do
                f1 <- genIntList 16
                f2 <- genIntLists (n-1)
                return (f1:f2)


-- Modified test framework from Week 2 code

test :: Int -> ([Int] -> Bool) -> [[Int]] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n p (f:fs) = 
  if (p f)
  then do print ("pass on:" ++ show f)
          test n p fs
  else error ("failed test on:" ++ show f)

testSets :: Int -> ([Int] -> Bool) -> IO ()
testSets n prop = do 
  fs1 <- genIntLists n
  test n prop fs1

-- Testable properties compare sort result between different algorithms
-- Time spend 30 min

srt1 = testSets 10 (\x  -> sort x == (mergeSortA  x))
srt2 = testSets 10 (\x  -> (splitSortA) x == (mergeSortA  x))
srt3 = testSets 10 (\x  -> (splitSortA) x == (sort  x))













