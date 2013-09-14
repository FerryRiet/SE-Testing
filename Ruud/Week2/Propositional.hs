module Week2.Propositional 

where
	
import Data.List

type Name = Int

data Form
	= Prop Name
	| Neg Form
	| Cnj [Form]
	| Dsj [Form]
	| Impl Form Form
	| Equiv Form Form
	deriving Eq

p = Prop 1
q = Prop 2
r = Prop 3

-- form1 and form3 are commented because they are tautologies
form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

-- Tautologies [list of function found at: http://www.millersville.edu/~bikenaga/math-proof/truth-tables/truth-tables.html]
-- 1. The law of the excluded middle.
taut1 = Dsj [Neg p, p] 
-- 2. Contradiction.
taut2 = Neg (Cnj [p, Neg p])
-- 3. Modus tolens
taut3 = Impl (Cnj [Impl (p) (q),Neg q]) (Neg p)
-- 4. Double negation
taut4 = Equiv (Neg (Neg p)) (p)
-- 5. Law of syllogism
taut5 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
-- 6. Decomposing a conjuntion
taut6  = Impl (Cnj [p, q]) (p)
taut6' = Impl (Cnj [p, q]) (q)
-- 7. Constructing a disjunction
taut7  = Impl (p) (Dsj [p, q])
taut7' = Impl (q) (Dsj [p, q])
-- 8. Definition of the biconditional
taut8 = Equiv (Equiv p q) (Cnj [Impl p q, Impl q p])
-- 9. Communicative law for ^
taut9 = Equiv (Cnj [p, q]) (Cnj [q, p])
-- 10. Communicative law for V
taut10 = Equiv (Dsj [p, q]) (Dsj [q, p])
-- 11. Associative law for ^
taut11 = Equiv (Cnj [Cnj [p, q], r]) (Cnj [p, Cnj [q, r]])
-- 12. Associative law for V
taut12 = Equiv (Dsj [Dsj [p, q], r]) (Dsj [p, Dsj [q, r]])
-- 13. DeMorgan's law
taut13 = Equiv (Neg (Cnj [p, q])) (Dsj [(Neg p), (Neg q)])
-- 14. DeMorgan's law
taut14 = Equiv (Neg (Dsj [p, q])) (Cnj [(Neg p), (Neg q)])
-- 15. Distributivity
taut15 = Equiv (Cnj [p, Dsj [q, r]]) (Dsj [Cnj [p, q], Cnj [p, r]])
-- 16. Distributivity
taut16 = Equiv (Dsj [p, Cnj [q, r]]) (Cnj [Dsj [p, q], Dsj [p, r]])
-- 17. Contrapostive
taut17 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
-- 18. Conditional disjunction
taut18 = Equiv (Impl p q) (Dsj [Neg p, q])
-- 19. Disjuntive syllogism
taut19 = Impl (Cnj [Dsj [p, q], Neg p]) (q)
-- 20. Simplification
taut20 = Equiv (Dsj [p, p]) (p)
-- 21. Simplification
taut21 = Equiv (Cnj [p, p]) (p)
-- 22. 23. Two others
taut22 = Impl (Cnj [Impl (Neg p) q, Impl (Neg p) (Neg q)]) (p) -- reductio ad absurdum.
taut23 = Impl (Cnj [(Dsj [p, q]), (Impl p r), (Impl q r)]) (r)-- proof by cases.

-- Contradictions
cont1 = Cnj [p, Neg p] 
cont2 = Neg (Impl p p)

-- Satisfiable (ofcourse, all tautologies are also satisfiable)
satis1 = p
satis2 = Neg p
satis3 = Impl p q

-- Source: http://homepages.cwi.nl/~jve/books/pdfs/lai.pdf (p 99.)
equiv1a = p
equiv1b = Neg $ Neg p
equiv2a = Neg p
equiv2b = Impl p (Cnj [q, Neg q])
equiv3a = Neg (Cnj [p, q])
equiv3b = Dsj [Neg p, Neg q]
equiv4a = Neg (Dsj [p, q])
equiv4b = Cnj [Neg p, Neg q]

type Valuation = [(Name, Bool)]

-- all possible valuations for list of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) =
	map 	((name, True) 	:) (genVals names)
	++ map 	((name, False) 	:) (genVals names)

-- generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

instance Show Form where
	show (Prop x)		= show x
	show (Neg f)		= '-' : show f
	show (Cnj fs)		= "*(" ++ showLst fs ++ ")"
	show (Dsj fs)		= "+(" ++ showLst fs ++ ")"
	show (Impl f1 f2)	= "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
	show (Equiv f1 f2)	= "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ' : show f ++ showRest fs

propNames :: Form -> [Name]
propNames = sort.nub.pnames where
	pnames (Prop name)		= [name]
	pnames (Neg f)			= pnames f
	pnames (Cnj fs)			= concat (map pnames fs)
	pnames (Dsj fs)			= concat (map pnames fs)
	pnames (Impl f1 f2)		= concat (map pnames [f1, f2])
	pnames (Equiv f1 f2)	= concat (map pnames [f1, f2])

eval :: Valuation -> Form -> Bool
eval [] (Prop c)	= error ("no info: " ++ show c)
eval ((i,b):xs) (Prop c)
	| c == i 	= b
	| otherwise	= eval xs (Prop c)
eval xs (Neg f)			= not (eval xs f)
eval xs (Cnj fs)		= all (eval xs) fs
eval xs (Dsj fs)		= any (eval xs) fs
eval xs (Impl f1 f2)	= not (eval xs f1) || eval xs f2
eval xs (Equiv f1 f2)	= eval xs f1 == eval xs f2

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> eval v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = all (\ x -> fst(x) == False || snd(x) == True)
	(zip (truthTable f1) (truthTable f2))

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\ x -> fst(x) == snd(x)) 
	(zip (truthTable f1) (truthTable f2))

truthTable :: Form -> [Bool]
truthTable f = map (\ v -> eval v f) (allVals f)


-- Joined variables
allTaut = [taut1, taut2, taut3, taut4, taut5, taut6', taut7, taut7', taut8, taut9, taut10, taut11, taut12, taut13, taut14, taut15, taut16, taut17, taut18, taut19, taut20, taut21, taut22, taut23]
allCont = [cont1, cont2]
allSatis = [satis1, satis2, satis3]

-- Tests:
testT1 = all tautology allTaut
testT2 = all satisfiable [taut1, taut2, taut3, taut4, taut5, taut6', taut7, taut7', taut8, taut9, taut10, taut11, taut12, taut13, taut14, taut15, taut16, taut17, taut18, taut19, taut20, taut21, taut22, taut23] 
testT3 = not (any contradiction [taut1, taut2, taut3, taut4, taut5, taut6', taut7, taut7', taut8, taut9, taut10, taut11, taut12, taut13, taut14, taut15, taut16, taut17, taut18, taut19, taut20, taut21, taut22, taut23]) 

testC1 = all contradiction [cont1, cont2]
testC2 = not (any satisfiable [cont1, cont2])

testS1 = all satisfiable [satis1, satis2, satis3]
testS2 = not (any contradiction [satis1, satis2, satis3])
testS3 = not (any tautology [satis1, satis2, satis3])

allPropositionalTests = all (\x -> x) [testT1, testT2, testT3, testC1, testC2, testS1, testS2, testS3]