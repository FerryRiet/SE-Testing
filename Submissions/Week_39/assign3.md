## Assignment Week 3

Creating a Int list with random integers. Time spend on this assignment 20 minutes, caused
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
only two empty sets left then and only then the set2 was a permutation od set1 

*Time spend 30 minutes*

``` Haskell
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (f:fs) (g:gs)  
        | f == g = isPermutation fs gs
        | otherwise = isPermutation fs gs'
                where gs' = (delete f (g:gs))
```

### Testable properties of the isPermutation function:
1: Length of input sets is equal
2: Sorted input sets are equal
3: nub of both sets are equal
4: Every element in set1 exists in set2

Below is a test for the isPermutation function

``` Haskell
testIspermutation :: [Int] -> Bool
testIspermutation fs = all ( \x -> isPermutation x fs) (permutations fs) 
```

## Testing the CNF function with the form generator

### First test run:

Gave almost immediately the following error:

`*** Exception: Cnf.hs:(34,1)-(35,31): Non-exhaustive patterns in function dist`

### Reasons

1. The form generator created both disjunctions and conjunctions with an empty list.
2. In the Dist algorithm is no case form Disjunctions with an empty list.

### Solutions

1. Fix the generator to generate expressions with a minimum of two arguments
2. Fix the Dist function to accept empty lists 
3. Modify CNF' not to call dist with an empty list

I choose to double fix the generator not to generate empty expressions, and modified 
CNF' not to call Dist with an empty list.

Added `cnf' (Dsj []) = Dsj []` alternative allowing empty disjunctions and returning them,
and added 2 to the random value of the generator.

The reason for the modification is that within our definition of Form the constants of True
and False (Dsj [] = false, Cnj [] = true) are not defined, and a rewrite of these empty operators 
to there constant equivalent is not possible but would be the best solution. 

*Times spend to fix 45 minutes*

``` Haskell
testRandomCNF :: IO Bool

testRandomCNF = 
        do f <- getRandomF
           return (equiv f (cnf f))

testMilCNF= all (\x -> equiv x (cnf x)) [ unsafePerformIO getRandomF | x <- [1..1000]]

tfs = testForms 1000 (\x -> equiv (x) (cnf x)) 

```

### Final test run

After the run of tfs we got:

         ....
         "pass on:Cnj [q,q,Neg q]"
         "pass on:s"
         "pass on:Dsj [Dsj [p,q,t],Dsj [t,q,q,s],t]"
         "pass on:Cnj [q,Cnj [t,p]]"
         "pass on:Dsj [Neg p,Cnj [q,s,s,p],Cnj [t,r,t],t]"
         "pass on:Dsj [Neg p,Neg t,r,Neg s]"
         "pass on:Cnj [Dsj [q,p],Dsj [q,t]]"
         "pass on:Dsj [q,p,Neg p]"
         "1000 tests passed"


*(note: the output of Show Form is adapted to input syntax)*

## Final

To extract the Haskell code from this document use the script called `md2hs` , the 
readable form of this file is rendered by the github "md" engine.
