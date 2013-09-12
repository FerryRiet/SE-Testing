module Week2.Triangle 

where

import Data.List

data Shape 
	= NoTriangle	-- invalid triangle 
	| Equilateral	-- 3 sides of the same lenght
	| Isosceles 	-- 2 sides of the same lenght
	| Rectangular	-- is a pythagorean triangle 
	| Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangle' . sort $ [a,b,c]

triangle' :: [Integer] -> Shape
triangle' [a,b,c]
	| isInvalid = NoTriangle 
	| allAreEqual = Equilateral 
	| twoAreEqual = Isosceles 
	| isPythagorean = Rectangular
	| otherwise = Other
	where
		positiveInts = all (>0) [a,b,c]
		isInvalid = positiveInts && a + b <= c 
		allAreEqual = length (nub([a, b, c])) == 1
		twoAreEqual = length (nub([a, b, c])) == 2
		isPythagorean = a^2 + b^2 == c^2
 
-- triangle
-- Time spent += 1 hour
-- Tests done:
--
-- *Week2> triangle 10 5 2
-- NoTriangle
-- *Week2> triangle 10 10 10
-- Equilateral
-- *Week2> triangle 10 10 9
-- Isosceles
-- *Week2> triangle 3 4 5
-- Rectangular
-- *Week2> triangle 16 8 20
-- Other
-- *Week2> triangle 16 12 20
-- Rectangular
-- *Week2> triangle 4 9 7
-- Other
-- *Week2> triangle 43 19 27
-- Other

-- Created functions to test... not sure how to use them
testNoTriangle 	:: [Integer] -> Bool
testNoTriangle 	(a:b:c:xs) = triangle a b c == NoTriangle

testEquilateral :: [Integer] -> Bool
testEquilateral	(a:b:c:xs) = triangle a b c == Equilateral

testIsosceles 	:: [Integer] -> Bool
testIsosceles	(a:b:c:xs) = triangle a b c == Isosceles

testRectangular :: [Integer] -> Bool
testRectangular	(a:b:c:xs) = triangle a b c == Rectangular

testOther 		:: [Integer] -> Bool
testOther		(a:b:c:xs) = triangle a b c == Other

-- *Week2> length (filter (testEquilateral) (map (replicate 3) [-10..100]))
-- 100
