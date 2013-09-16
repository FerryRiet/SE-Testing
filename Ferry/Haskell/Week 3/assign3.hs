import Week3
import Techniques 
import Data.List 

genIntList :: IO [Int]
genIntList = genIntList' 10

genIntList'  :: Int -> IO [Int]
genIntList'  0 =  return [] 
genIntList'  n =  do c <- getRandomInt 10
                     l <- genIntList' (n-1)
                     return (c:l)


isPermutaion :: Eq a => [a] -> [a] -> Bool
isPermutaion a b =  any (\ a -> a == b ) c
					where c = permutations a