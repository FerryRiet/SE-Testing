-- isTriangle :: [int] -> bool

import Data.List

data Shape = NoTriangle 
		   | Equilateral
           | Isosceles 
           | Rectangular 
           | Other 
           deriving (Eq,Show)


isTriangle :: Integer -> Integer -> Integer -> Shape

isTriangle a b c  |  (a + b < c || b + c < a || c + a < b) = NoTriangle
				  |  a == b && a == c  = Equilateral
				  |  a == c || b == c  || a == b = Isosceles
				  |  (a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2) = Rectangular 
                  |  otherwise = Other


-- Testing approach
-- Create input test sets  which will evaluate every booliean sub expression both True and
-- False.  For the NoTriangle, Isosceles and Rectangular line need 8 input sets the Equilateral 
-- line needs 4 different sets.   

-- 
t1 = [ isTriangle a b c | a <- [2..5] , b <- [2..5] , c <- [2..5]]
t2 = [ isTriangle a b c | a <- [2..5] , b <- [2..5] , c <- [2..5], length (nub [a,b,c]) == 1 ]
t3 = [ isTriangle a b c | a <- [2..5] , b <- [2..5] , c <- [2..5], length (nub [a,b,c]) == 2 ]
