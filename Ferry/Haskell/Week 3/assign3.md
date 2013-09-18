``` Haskell
import Week3
import Techniques 
import Data.List 
import Cnf
import System.IO.Unsafe
import Logic


-- Time spend +20 min

genIntList :: IO [Int]
genIntList = genIntList' 10	

genIntList'  :: Int -> IO [Int]
genIntList'  0 =  return [] 
genIntList'  n =  do c <- getRandomInt 10
                     l <- genIntList' (n-1)
                     return (c:l)

{-
	Testable properties:
	1: Length of input sets is equal
	2: Sorted input sets are equal
	3: nub of both sets are equal
-}

-- Time spend 10 min

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation set1 set2  
				| length set1 /= length set2 = False
--					| sort set1 == sort set2     = True
				| otherwise    = any (\ x -> x == set2 ) permlist
					             where permlist = permutations set1

testIspermutation :: [Int] -> Bool
testIspermutation fs = all ( \x -> isPermutation x fs) (permutations fs) 

testRandomCNF :: IO Bool
testRandomCNF = 
	do f <- getRandomF
	   return (equiv f (cnf f))

testMilCNF= all (\x -> equiv x (cnf x)) [ unsafePerformIO getRandomF | x <- [1..1000]]

tfs = testForms 100 (\x -> equiv (x) (cnf x)) 

```
