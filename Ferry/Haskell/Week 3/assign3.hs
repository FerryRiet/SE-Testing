import Week3
import Techniques 

genIntList :: IO [Int]
genIntList = genIntList' 10

genIntList'  :: Int -> IO [Int]
genIntList'  0 =  return [] 
genIntList'  n =  do c <- getRandomInt 10
                     l <- genIntList' (n-1)
                     return (c:l)