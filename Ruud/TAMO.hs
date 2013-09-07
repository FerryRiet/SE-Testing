module TAMO

where

(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> x = True 

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y


valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && (bf False)

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = (bf True True) 
		 && (bf True False)
		 && (bf False True)
		 && (bf False False)A

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [True,False],
							 q <- [True,False],
							 r <- [True,False]]
valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [ bf p q r s | p <- [True,False],
							   q <- [True,False],
							   r <- [True,False],
							   s <- [True,False]]
excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

p = True 
q = False

formula1 = (not p) && (p ==> q) <=> not (q && (not p))
formula2 p q = ((not p) && (p ==> q) <=> not (q && (not p)))

form1 p q = p ==> (q ==> p)
form2 p q = (p ==> q) ==> p