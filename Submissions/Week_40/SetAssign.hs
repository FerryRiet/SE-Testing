{-
   Lab assignment 4
   Author Ferry (2013-9-24)
-}

module SetAssign
where

import System.Random
import Data.List 
import SetOrd


{-
  Generate random set
  Time spend +- 60 minutes
  Version 1
-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomSet :: IO (Set Int)
randomSet = do
             s <- randomSet' 10 
             return (s)

randomSet' :: Int -> IO (Set Int) 
randomSet' 0 = return (Set [])
randomSet' x = do
                c <- getRandomInt 30
                l <- randomSet' (x-1)
                return (insertSet c l)


{- 
   Version 2 with external genIntList    
-}

genIntList :: IO [Int]
genIntList = genIntList' 8 

genIntList'  :: Int -> IO [Int]
genIntList'  0 =  return [] 
genIntList'  n =  do c <- getRandomInt 16
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

     
rss :: Int -> IO [Set Int]
rss 0 = return ([])  
rss n = do 
         f <- rs 
         fs <- rss (n-1) 
         return (f:fs)
