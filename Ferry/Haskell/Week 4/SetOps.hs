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
        print ( show r ++ " SetInt " ++ show r2 ++  " => " ++ show r3)
        return ( r == (setInt r r2))

testDif :: IO Bool
testDif = do
        r <- rs
        print ( show r )
        return ( r /= (setDif r r))
