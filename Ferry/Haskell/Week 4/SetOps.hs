import Data.List
import SetOrd
import SetAssign

--setUnion :: (Ord a) => Set a -> Set a -> Set a
--setUnion s1 s2 = s2

setInt :: (Ord a) => Set a -> Set a -> Set a
setInt (Set [])  _ = Set []
setInt _  (Set []) = Set []
setInt (Set (s:sy)) s2 
        |  inSet s s2 = insertSet s (setInt (Set sy) s2)
        |  otherwise  = (setInt (Set sy) s2)  

setDif :: (Ord a) => Set a -> Set a -> Set a
setDif (Set []) _      = Set []
setDif _  (Set [])     = Set []
setDif (Set (s:sy))  s2 
        | inSet s s2 = (setDif (Set sy) s2)         
        | otherwise  = insertSet s (setDif (Set sy) s2)

testInt :: IO Bool
testInt = do
        r  <- rs
        let r2 = (insertSet 99 r)
        let r3 = setInt r r2
        print  ( show r ++ " SetInt " ++ show r2 ++  " => " ++ show r3)
        return ( r == (setInt r r2))

testDif :: IO Bool
testDif = do
        r <- rs
        print ( show r )
        return ( r /= (setDif r r))


test :: Int -> (Set Int -> Bool) -> [Set Int] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n p (f:fs) = 
  if p f 
  then do print ("pass on:" ++ show f)
          test n p fs
  else error ("failed test on:" ++ show f)

testSets :: Int -> (Set Int -> Bool) -> IO ()
testSets n prop = do 
  fs <- rss n
  test n prop fs

dif1 = testSets 1000 (\x -> (Set [98,99]) == (setDif (insertSet 98 (insertSet 99 x))  x))
dif2 = testSets 1000 (\x -> (Set []) == (setDif x x))
int1 = testSets 1000 (\x -> x == (setInt (insertSet 99 x) x))
int2 = testSets 1000 (\x -> x == (setInt x (insertSet 99 x)))

main = print ("Hello world\n")

