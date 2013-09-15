module AssignmentWeek2 where
import Data.List
import Week2

{- 
	Assignment 1: triangles 
	Time: 1,5 hour (incl reading Wikipedia)
-}
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangle2 (sort [a,b,c])

triangle2 :: [Integer] -> Shape
triangle2 (a:b:c:xs)	| a + b <= c 		= NoTriangle
			| a == b && b == c	= Equilateral
		        | a^2 + b^2 == c^2 	= Rectangular
			| a == b || b == c 	= Isosceles
			| otherwise 		= Other

testDataTriangles = [[a,b,c] | a <- [0..10], b <- [1..10], c <- [1..10]] 

-- Generate a list of a results
testTriangles1 :: [[Integer]] -> [Shape]
testTriangles1 xs = [(triangle a b c) | (a:b:c:x) <- xs]
-- Get all the numbers for a kind of triangle
testTriangles2 :: Shape -> [[Integer]]
testTriangles2 shape = [[a,b,c] | (a:b:c:xs) <- testDataTriangles, shape == triangle a b c]

{- 
	Assignment 2: contradiction, tautology, logical entailment and logical equivalence 
	Time: 2,5 hour
-}
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

entails :: Form -> Form -> Bool
entails frm1 frm2 = tautology (Impl frm1 frm2)

equiv :: Form -> Form -> Bool
equiv frm1 frm2 = (entails frm1 frm2) && (entails frm2 frm1)

testData :: [Form]
testData =  [Equiv (Impl (Cnj [p, q]) r) (Impl p(Impl q r)), Cnj [p, (Neg p)], Dsj [p, (Neg q)], Dsj [p, Neg p]]
testData2 :: [Form]
testData2 = [Equiv (Impl (Cnj [p, q]) r) (Impl p(Impl q r))]
{-
    Assignment 3: CNF
    I just followed the information from the lecture sheets
    Time: 0,5 hour
-}
cnf :: Form -> Form
cnf frm = cnf2 (nnf (arrowfree frm))

cnf2 :: Form -> Form
cnf2 (Prop frm)         = Prop frm
cnf2 (Cnj frm)          = Cnj (map cnf2 frm)
cnf2 (Dsj (f:frm))      = dist (cnf2 f) (cnf2 (Dsj frm))
cnf2 (Neg frm)          = Neg frm
cnf2 (Dsj [])           = Dsj []

dist :: Form -> Form -> Form
dist (Cnj (f1:frm1)) frm2 = Cnj [dist f1  frm2, dist  (Cnj frm1) frm2]
dist frm1 (Cnj (f2:frm2)) = Cnj [dist frm1 f2, dist frm1 (Cnj frm2)]
dist frm1 frm2            = Dsj [frm1, frm2]
