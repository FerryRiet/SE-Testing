module Cnf

where 

import Week2
import Logic

--------------------------

-- precondition: input should be a valid form
-- postcondition: CNF (a) returns CNF of a
cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree -- Don't listen to Jimmy 

-- precondition: input forms is in nnf
-- postcondition: CNF (NNF) returns CNF of NNF
cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg  x) = Neg x        -- Do listen to Jimmy
cnf' (Cnj fs) = Cnj (map cnf' fs)  
cnf' (Dsj fs) = dist (map cnf' fs)

-- precondition: input form is in cnf
-- postcondition: returns form in cnf with the laws of distribution applied
dist' :: Form -> Form ->Form
dist' (Cnj f) p = Cnj (map (\x -> dist' x p) f)    
dist' f (Cnj p) = Cnj (map (\x -> dist' f x) p)
dist' f p = Dsj [f, p]  

-- precondition: all input forms are in cnf
-- postcondition: returns form in cnf with the laws of distribution applied
dist :: [Form] -> Form
dist [f] = f
dist (f:fs) = dist' f (dist fs)

-- precondition: a valid form
-- postcondition: returns true if "valid form" is equivalent with CNF("valid form")
testCNF :: Form -> Bool
testCNF f = equiv f g
		    where g = cnf f


t1 = testCNF (Dsj [Dsj [p,q],q])

{-
Coding the first version took +- 3 hours, id did not work at all 
missing was the Neg operator from the CNF algorithm. Took me 1 hour to
understand the Haskell not matching error, and the missing case.
-}

{- Dist check from lecture slides
First case uses equivalence of (p ^ q) _ r and (p _ r) ^ (q _ r).
Second case uses equivalence of p _ (q ^ r) and (p _ q) ^ (p _ r).
-}

dc1 = dist' p (Cnj [q, r]) == Cnj [Dsj [p, q], Dsj [p, r]]
dc2 = dist' (Cnj [q, r]) p == Cnj [Dsj [q, p], Dsj [r, p]]

{-
Testing

run test set composed of satisfiables, contradictions, and tautologies and test if there CNF is equivalent
with the original function.

-}

distResults = all (\x -> x) [dc1,dc2]

cnfResults = all testCNF [s1,s2,s3,s4,s5,s6,c1,c2,c3,c4,Logic.t1,t2,t3,t4,t5,t6]

--nodist :: (Form,Form) -> Form
--nodist Cnj f j   = j
--nodisk f Cnj j  = f
--nodisk f j       = Dsj f j
