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


rel1 = [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
rel2 = [(2,1)]

rel3 = [(1,3),(3,1)]
rel4 = [(1,2),(2,3)]

rel5 = [(1,2),(2,3),(3,1)]
rel6 = [(1,2),(2,3),(4,5)]


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


testTrClos :: IO Bool
testTrClos = do
        r <- getRandomRel
        let r2 = trClos r
        let rs = intersect r r2
        let bl = transR r2
        print ( show r ++  ">> trClos >>" ++ show r2 ++ " intersection " ++ show rs )
        return ( r == rs && bl )


 


