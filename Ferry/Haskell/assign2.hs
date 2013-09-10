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

cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree 

cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg  x) = Neg x
cnf' (Cnj fs) = Cnj (map cnf' fs)
cnf' (Dsj fs) = dist (map cnf' fs)

dist' :: Form -> Form ->Form
dist' (Cnj f) p = Cnj (map (\x -> dist' x p) f)
dist' f (Cnj p) = Cnj (map (\x -> dist' f x) p)
dist' f p = Dsj [f, p]  

dist :: [Form] -> Form
dist [f] = f
dist (f:fs) = dist' f (dist fs)

testCNF :: Form -> Bool
testCNF f = equiv f g
		    where g = cnf f

--nodist :: (Form,Form) -> Form
--nodist Cnj f j   = j
--nodisk f Cnj j  = f
--nodisk f j       = Dsj f j