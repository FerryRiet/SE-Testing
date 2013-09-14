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

-- First Testing approach
-- Create input test sets which will evaluate every booliean sub expression both True and
-- False.  For the NoTriangle, Isosceles and Rectangular line need 8 input sets the Equilateral 
-- line needs 4 different sets.   
-- 
-- Second approach
-- Create full set and test output.

t1 = [ isTriangle a b c | a <- [2..5] , b <- [2..5] , c <- [2..5]]
t2 = [ isTriangle a b c | a <- [2..5] , b <- [2..5] , c <- [2..5], length (nub [a,b,c]) == 1 ]
t3 = [ isTriangle a b c | a <- [2..5] , b <- [2..5] , c <- [2..5], length (nub [a,b,c]) == 2 ]
t4 = [ isTriangle a b c | a <- [2..5] , b <- [2..5] , c <- [2..5], a + b < c || b + c < a || c + a < b ]

-- Test NoTriangle 3 in set
cnot = length (filter (== True) $ map (== NoTriangle ) t1)  == 3    -- 225,252,522

-- Test Rectanggular 6 in set
crct = length (filter (== True) $ map (== Rectangular ) t1) == length (permutations [3,4,5]) -- 6

-- Test Equilateral 4 in set
ceql = length (filter (== True) $ map (== Equilateral ) t1) == 4   -- 222,333,444,5555

-- Combinations twice  2 out 4 = 16 minus the Equil = 12, select 1 from selection and permutate 
-- gives 72, remove doubles, and remove NoTriangles leaves 33 Isosceles triangles   
ciso = length (filter (== True) $ map (== Isosceles ) t1)  ==  33  -- Combinations twice  2 out 4 = 16 minus the Rects ==12 

-- The rest test
coth = length (filter (== True) $ map (== Other ) t1)  == 18       -- rest

-- Time spend:

-- Writing first version took me +- 10 minutes. After that a modification and rearanging
-- the guards took me an other 10 minutes.
--
-- The testing.
-- I knew what I wanted to do