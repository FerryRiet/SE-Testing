-- put as first line in TAMO.hs: {-# LANGUAGE FlexibleInstances #-}
module Sol2 where
import TAMO

-- Exercise 2.12
theorem1 = not True <=> False
theorem2 = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
theorem3 = logEquiv1 (\ p -> p || True) (const True)
theorem4 = logEquiv1 (\ p -> p || True) id
theorem5 = logEquiv1 (\ p -> p || not p) (const True)
theorem6 = logEquiv1 (\ p -> p && not p) (const False)
-- Exercise 2.15
contradiction :: (Bool -> Bool) -> Bool
contradiction bf = not (bf True) && not (bf False)

contradiction2 :: (Bool -> Bool -> Bool) -> Bool
contradiction2 bf = and [not (bf p q) | p <- [True, False],
					q <- [True, False]]
contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contradiction3 bf = and [not (bf p q r) | 	p <- [True, False],
						q <- [True, False],
						r <- [True, False]]
-- Exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

-- Exercise 2.52
parity :: [Bool] -> Bool
parity xs = mod (length (filter (==True) xs)) 2 == 0

-- Exercise 2.53
evenNr :: (a -> Bool) -> [a] -> Bool
evenNr p = parity . map p
