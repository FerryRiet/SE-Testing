module Clos

where

import Data.List


type   Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s =
     [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


rel1 = [(3,2)]
rel2 = [(2,1)]

rel3 = [(2,2),(3,4)]
rel4 = [(1,2),(2,3)]


--trClos :: Ord a => Rel a -> Rel a
--trClos 