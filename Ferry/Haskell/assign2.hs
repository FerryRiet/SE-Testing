
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

--cnf :: Form -> Form
--cnf = nnf . arrowfree 

cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = dist (map cnf fs)

dist :: (Form,Form) -> Form
dist Cnj f j   = j
disk f Cnj j  = f
disk f j       = Dsj f j