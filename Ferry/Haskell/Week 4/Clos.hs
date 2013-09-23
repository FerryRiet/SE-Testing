module Clos

where

import Data.List


type   Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


rel1 = [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
rel2 = [(2,1)]

rel3 = [(1,3),(3,1)]
rel4 = [(1,2),(2,3)]

trClos :: Ord a => Rel a -> Rel a
trClos s = nub (sort (s ++ (s @@ s)))  