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



