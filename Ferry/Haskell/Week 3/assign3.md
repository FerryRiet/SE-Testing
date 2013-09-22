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
removes element by element if they are the same in both sets, and if at the end there are 
only two empty sets left then an only then the set2 was a permutation od set1 

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

Testing the CNF function with the form generator

## First test run:

`*** Exception: Cnf.hs:(34,1)-(35,31): Non-exhaustive patterns in function dist`

## Reason

1. The form generatr created both disjunctions and conjunctions with ean empty list.
2. In the Dist algoritme is no case form Disjunctions with an empty list.

## Solutions

1. Fix the generator to genarate exprsssions with a minimum of two arguments
2. Fix the Dist function to accept empty lists 
3. Modify CNF' not to call dist with an empty list

I choose to fix the generator not to generate empty expressions, and modified 
CNF' not to call Dist with an empty list

Added `cnf' (Dsj []) = Dsj []` alternative and added 2 to the random value of the generator.


``` Haskell
testRandomCNF :: IO Bool

testRandomCNF = 
        do f <- getRandomF
           return (equiv f (cnf f))

testMilCNF= all (\x -> equiv x (cnf x)) [ unsafePerformIO getRandomF | x <- [1..1000]]

tfs = testForms 1000 (\x -> equiv (x) (cnf x)) 

```

# Finally

After the run of tfs we got

`"pass on:Cnj [q,q,Neg q]"
"pass on:s"
"pass on:Dsj [Dsj [p,q,t],Dsj [t,q,q,s],t]"
"pass on:Cnj [q,Cnj [t,p]]"
"pass on:Dsj [Neg p,Cnj [q,s,s,p],Cnj [t,r,t],t]"
"pass on:Dsj [Neg p,Neg t,r,Neg s]"
"pass on:Cnj [Dsj [q,p],Dsj [q,t]]"
"pass on:Dsj [q,p,Neg p]"
"1000 tests passed"`


