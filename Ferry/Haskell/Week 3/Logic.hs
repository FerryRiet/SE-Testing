module Logic

where

import Week2

contradiction :: Form -> Bool
contradiction f = not (any (\ v -> eval v f) (allVals f))

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical entailment
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)	

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

truthTable :: Form -> [Bool]
truthTable f = map (\ v -> eval v f) (allVals f)

--- Logic tests

s1 =(Dsj [(Cnj [ q, Neg p]), (Cnj [ Neg q,p])]) -- ExOR
s2 = Dsj [p,q]
s3 = Cnj [p,q]
s4 = Neg p
s5 = Impl  p q
s6 = Equiv p q      -- False

-- Contradictions
c1 = Cnj [Neg p,p]
c2 = Neg (Dsj [Neg p,p])
c3 = Neg t2
c4 = Equiv p (Neg p)

-- Tautology's
t1 = Dsj [Neg p,p]	 
t2 = Neg (Cnj [p, Neg p])
t3 = Equiv t1 t1
t4 = Impl  t3 t3
t5 = form1
t6 = form3

e1 = entails (p) (Dsj [p, q])           -- True
e2 = entails (Dsj [p, q]) (Dsj [q, p])  -- True
e3 = entails (p) (Cnj [p, q])           -- False
e4 = entails (Dsj [p, q]) (p)           -- False

testSatisfiable   = all satisfiable [s1]

testContradiction    = all contradiction [c1,c2,c3,c4]
testNotContradiction = not $ any contradiction [t1,t2,t3,t4,t5,t6]

testTautology     = all tautology [t1,t2,t3,t4,t5,t6]
testNotTautology  = not $ any tautology ([c1,c2,c3,c4] ++ [s1])

testEntails		  = all (\x -> x )  [e1,e2]
testNotEntails	  = not $ any  (\x -> x )  [e3,e4]


-- Time spend
-- After reading the collage sheets and the Week2.hs file it took me 10 minutes to make the
-- four functions. Reading-time approx 45 minutes
-- The testing took 1 hour

-- Testing
-- First step was testing the Eval functions so I created s1...s6 and checked the truth tables
-- The functions were created so that each pattern in the functions is matched (code coverage)

-- Step 2: Create known Contradictions an Tautology's and run both tests on both sets of functions

-- Step 3: Created two sets of entails (T,F) and run bots tests 
--

