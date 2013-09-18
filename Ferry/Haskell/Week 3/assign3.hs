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

remFst :: Eq a =>  a -> [a] -> [a]
remFst n [] = []
remFst n (f:fs) 
        |  n == f = fs
        |  otherwise = [f] ++ (remFst n fs)   

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True

isPermutation (f:fs) g  
				| length (f:fs) /= length g = False
				| otherwise    = isPermutation s1 s2
					             where s1  = fs
					                   s2  = remFst f g


testIspermutation :: [Int] -> Bool
testIspermutation fs = all ( \x -> isPermutation x fs) (permutations fs) 

testRandomCNF :: IO Bool
testRandomCNF = 
	do f <- getRandomF
	   return (equiv f (cnf f))

testMilCNF= all (\x -> equiv x (cnf x)) [ unsafePerformIO getRandomF | x <- [1..1000]]

tfs = testForms 100 (\x -> equiv (x) (cnf x)) 
