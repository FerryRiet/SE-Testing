module SetAssign
where

import System.Random
import Data.List 
import SetOrd


-- Time spend +- 60 minutes
-- Version 1
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomSet :: IO (Set Int)
randomSet = do
             s <- randomSet' 10 
             return (s)

randomSet' :: Int -> IO (Set Int) 
randomSet' 0 = return (Set [])
randomSet' x = do
                c <- getRandomInt 10
                l <- randomSet' (x-1)
                return (insertSet c l)


-- Version 2 with external genIntList    
genIntList :: IO [Int]
genIntList = genIntList' 10 

genIntList'  :: Int -> IO [Int]
genIntList'  0 =  return [] 
genIntList'  n =  do c <- getRandomInt 30
                     l <- genIntList' (n-1)
                     return (c:l)

rs :: IO (Set Int)
rs = do
      c <- rs' 10
      return (c)

rs' :: Int -> IO (Set Int) 
rs' x = do 
            d <- genIntList' x
            return (Set (sort (nub d)))

