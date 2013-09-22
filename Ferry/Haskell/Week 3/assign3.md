# Assignment Week 3

Creating a Int list with random integers. Time spend on this assignment 20, caused
by the concept of unsafe IO. (e.g. monads)

``` Haskell
import Week3
import Techniques 
import Data.List 
import Cnf
import System.IO.Unsafe
import Logic

genIntList :: IO [Int]
genIntList = genIntList' 10     

genIntList'  :: Int -> IO [Int]
genIntList'  0 =  return [] 
genIntList'  n =  do c <- getRandomInt 10
                     l <- genIntList' (n-1)
                     return (c:l)

```

The implementation of the function isPermutation. This implementation 
removes element the same elements from both sets, and if at the end there are 
only two empty sets left then an only then sets set2 was a permutation od set1 

``` Haskell
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


```

Testable properties of the isPermutation function:
1: Length of input sets is equal
2: Sorted input sets are equal
3: nub of both sets are equal
4: Every element in set1 exists in set2

Below is a test for the isPermutation function

``` Haskell
testIspermutation :: [Int] -> Bool
testIspermutation fs = all ( \x -> isPermutation x fs) (permutations fs) 
```

Testing the CNF function  with the form generator

First test run:
Epic fail, 

reason

Solutions


``` Haskell
testRandomCNF :: IO Bool
testRandomCNF = 
        do f <- getRandomF
           return (equiv f (cnf f))

testMilCNF= all (\x -> equiv x (cnf x)) [ unsafePerformIO getRandomF | x <- [1..1000]]

tfs = testForms 100 (\x -> equiv (x) (cnf x)) 

```
