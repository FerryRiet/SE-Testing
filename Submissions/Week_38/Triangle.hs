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
		isInvalid = not positiveInts || (a + b <= c) 
		allAreEqual = length (nub([a, b, c])) == 1
		twoAreEqual = length (nub([a, b, c])) == 2
		isPythagorean = a^2 + b^2 == c^2

-- triangle
-- Time spent += 1 à 2 hour(s)

-- Manual Tests done:
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

-- Multiple tests:

-- Test invalid triangles
noTriangles = [[x,y,z] | x <- [-10,0..100], y <- [10,20..200], z <- [10+x+y,20+x+y..100+x+y]]
testNT = all (\(a:b:c:xs) -> triangle a b c == NoTriangle) noTriangles

-- Test triangles having 2 same lengths
equis  = [[x,y,z] | x <- [1..100], y <- [x], z <- [x]]
equis' = [[x,y,z] | x <- [-100,-99..0], y <- [x], z <- [x]]
testE = all (\(a:b:c:xs) -> triangle a b c == Equilateral) equis
testE' = all (\(a:b:c:xs) -> triangle a b c == NoTriangle) equis'

isos  = [[x,y,z] | x <- [20,21..100], y <- [x], z <- [x+y-10]]
isos2 = [[x,y,z] | x <- [20,21..100], z <- [x], y <- [x+z-10]]
testI = all (\(a:b:c:xs) -> triangle a b c == Isosceles) isos 
testI2 = all (\(a:b:c:xs) -> triangle a b c == Isosceles) isos2

rects = [[x,y,z] | x <- [1..100], y <- [1..x], z <- [x..100], x^2+y^2==z^2]
testR = all (\(a:b:c:xs) -> triangle a b c == Rectangular) rects

others = [[x,y,z] | x <- [1..100], y <- [1..x], z <- [x..100],
	not (x == y || x == z), not (x^2+y^2==z^2), not (x+y<=z)]
testO = all (\(a:b:c:xs) -> triangle a b c == Other) others

-- run all tests
allTriangleTests = all (\x -> x) [testNT, testE, testE', testI, testI2, testR, testO]