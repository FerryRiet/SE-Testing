module SetOps

where 

import Data.List
import SetOrd
import SetAssign

-- Union Set already in SetOrd.hs
-- Time spend 10 min
uniSet :: (Ord a) => Set a -> Set a -> Set a 
uniSet (Set [])     s2  =  s2
uniSet (Set (x:xs)) s2  = 
   insertSet x (uniSet (Set xs) s2)

-- Time spend 15 min
setInt :: (Ord a) => Set a -> Set a -> Set a
setInt (Set [])  _ = Set []
setInt _  (Set []) = Set []
setInt (Set (x:xs)) s2 
        |  inSet x s2 = insertSet x (setInt (Set xs) s2)
        |  otherwise  = (setInt (Set xs) s2)  


-- Time spend 20 min
setDif :: (Ord a) => Set a -> Set a -> Set a
setDif (Set []) _      = Set []
setDif _  (Set [])     = Set []
setDif (Set (s:sy))  s2 
        | inSet s s2 = (setDif (Set sy) s2)         
        | otherwise  = insertSet s (setDif (Set sy) s2)


-- Time spend 60 --
-- Modified test framework from Week 2 code

test :: Int -> (Set Int -> Set Int -> Bool) -> [Set Int] -> [Set Int] -> IO ()
test n _ [] _ = print (show n ++ " tests passed")
test n p (f:fs) (g:gs) = 
  if (p f g)
  then do print ("pass on:" ++ show f ++ " and " ++ show g)
          test n p fs gs
  else error ("failed test on:" ++ show f ++ " and " ++ show g)

testSets :: Int -> (Set Int -> Set Int -> Bool) -> IO ()
testSets n prop = do 
  fs1 <- rss n
  fs2 <- rss n
  test n prop fs1 fs2

-- Testable properties uniSet
-- Time spend 30 min
-- Test is union of sets is equal of the Haskell list concatenation and Set conversion


uni1 = testSets 10 (\(Set x) (Set (y)) -> list2set (x ++ y) == (uniSet (list2set x) (list2set y)))
uni2 = testSets 10 (\x _ -> x == (uniSet x (Set [])))

-- Test of SetDif
-- Test if difference between two Set with a known Difference equals the known diff.
-- Test test is the difference between two identical set results a empty set.  

dif1 = testSets 10 (\x _ -> (Set [98,99]) == (setDif (insertSet 98 (insertSet 99 x))  x))
dif2 = testSets 10 (\x _ -> (Set []) == (setDif x x))


-- Test of setIntercection
-- Test is run with dffferent input sets with a known intersections 
int1 = testSets 10 (\x _ -> x == (setInt (insertSet 99 x) x))
int2 = testSets 10 (\x _ -> x == (setInt x (insertSet 99 x)))
