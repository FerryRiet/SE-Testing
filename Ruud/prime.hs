-- Exercise 1.4
--
-- Question:	Suppose in the definition of ldf we replace the condition k^2 > n
-- 				by k^2 >= n, where >= expresses ‘greater than or equal’. Would that make any
-- 				difference to the meaning of the program? Why (not)?
--
-- Answer:		TODO

-- Exercise 1.6
-- 
-- Question:	Can you gather from the definition of divides what the type declaration
--				for rem would look like?
--
-- Answer:		TODO


-- Code from other exercises below:

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n 	| divides k n 	= k
			| k^2 > n		= n
			| otherwise		= ldf (k+1) n

prime0 :: Integer -> Bool 
prime0 n 	| n < 1 		= error "not a positive integer"
		 	| n == 1 		= False
		 	| otherwise		= ld n == n