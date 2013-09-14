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


--- Logic tests

s1 =(Dsj [(Cnj [ q, Neg p]), (Cnj [ Neg q,p])]) -- ExOR

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

e1 = entails (p) (Dsj [p, q])           {- True -}
e2 = entails (Dsj [p, q]) (Dsj [q, p])  {- True -}
e3 = entails (p) (Cnj [p, q])           {- False -}
e4 = entails (Dsj [p, q]) (p)           {- False -}


testSatisfiable   = all satisfiable [s1]

testContradiction    = all contradiction [c1,c2,c3,c4]
testNotContradiction = not $ any contradiction [t1,t2,t3,t4]

testTautology     = all tautology [t1,t2,t3,t4]
testNotTautology  = not $ any tautology [c1,c2,c3,c4]

testEntails		  = all (\x -> x )  [e1,e2]
testNotEntails	  = not $ any  (\x -> x )  [e3,e4]

truthTable :: Form -> [Bool]
truthTable f = map (\ v -> eval v f) (allVals f)