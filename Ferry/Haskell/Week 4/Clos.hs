module Clos

where

import Data.List
import SetOrd
import SetAssign
import SetOps

type   Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

invR :: Ord a => Rel a -> Rel a
invR [] = []
invR ((x,y):r) = [(y,x)] ++ invR r 

trClos :: Ord a => Rel a -> Rel a
trClos  s 
        | length s == (length s') = s
        | otherwise = trClos s'
        where s' = trClos' s

trClos' :: Ord a => Rel a -> Rel a
trClos' s = nub (sort (s ++ (s @@ s)))


-- TransR Copied from the book "The Haskell road to Logic,Maths and Programming"
-- Page 175. Used to test the output relation of trClos

transR :: Ord a => Rel a -> Bool
transR [] = True
transR  s = and [ trans pair s | pair <- s ]
            where 
                trans (x,y) r =  and [  elem (x,v) r |  (u,v) <- r , u ==y ]  

getRandomRel :: IO (Rel Int)
getRandomRel = do
                 l1 <- genIntList
                 l2 <- genIntList
                 return (sort (nub (zip l1 l2)))

getRandomRels :: Int -> IO ([Rel Int])
getRandomRels 0 = return [] 
getRandomRels n = do
                  l <- getRandomRel
                  l2 <- getRandomRels (n-1)
                  return (l:l2)

testTr :: Int -> (Rel Int  -> Bool) -> [Rel Int] -> IO ()
testTr n _ []  = print (show n ++ " tests passed")
testTr n p (f:fs) = 
                      if (p f)
                      then do print ("pass on:" ++ show f )
                              testTr n p fs
                      else error ("failed test on:" ++ show f)

testSetsTr :: Int -> (Rel Int -> Bool) -> IO ()
testSetsTr n prop = do 
                      fs1 <- getRandomRels n
                      testTr n prop fs1 

{- Testing 
    The first test, tests if the initial relation is still a contained in the Transitive closure.
    The second test, tests if the result of the trClos really is a Transitive closure.
    The final test removes a single entry from the relation an tests if the start relation still subset and 
    if the result is still a Transitive closure if so 

-} 
cl1 = testSetsTr 100 (\x   -> intersect x (trClos x) == x)
cl2 = testSetsTr 100 (\x   -> transR (trClos x))
cl3 = testSetsTr 100 (\is  -> 
                          not (
                              any ( \q -> transR q && (intersect is q == is) ) 
                                   [ z  | z <- [ delete (x,y) (trClos is) | (x,y) <- trClos is ]] 
                              )
                     ) 

-- Known relations
rel1 = [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
rel2 = [(2,1)]

rel3 = [(1,3),(3,1)]
rel4 = [(1,2),(2,3)]

rel5 = [(1,2),(2,3),(3,1)]
rel6 = [(1,2),(2,3),(4,5)]

