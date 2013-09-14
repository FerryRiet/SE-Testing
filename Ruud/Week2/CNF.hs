module Week2.CNF 

where

import Week2.Propositional


-- no precondition: should work for any formula. 
arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2
-- postcondition: output is arrow-free

-- precondition: input is arrow-free
nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
-- postcondition: NFF (f) returns NNF of f

-- precondition: input is arrow-free and in NNF
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f1:f2:fs)) = dist f1 f2
cnf f = f
-- postcondition: CNF (f) returns CNF of f

-- precondition: input are in CNF 
dist :: Form -> Form -> Form
dist (Cnj []) _ 		= Cnj []
dist (Cnj [f1]) f2 		= dist f1 f2
dist (Cnj (f1:fs)) f2	= Cnj [(dist f1 f2), (dist (Cnj fs) f2)]
dist _ (Cnj []) 		= Cnj []
dist f1 (Cnj [f2]) 		= dist f1 f2
dist f1 (Cnj (f2:fs)) 	= Cnj [(dist f1 f2), dist f1 (Cnj fs)]
dist f1 f2				= Dsj [f1,f2]
-- postcondition: DIST(f1,f2) returns CNF of Dsj [f1, f2]

-- Tests
fullCnf :: Form -> Form
fullCnf = cnf . nnf . arrowfree

testCnf :: Form -> Bool
testCnf f = equiv f (fullCnf f)

-- *Week2> arrowfree form1
-- +(*(+(-1 2) +(--2 -1)) *(-+(-1 2) -+(--2 -1)))
-- *Week2> equiv (form1) (arrowfree form1)
-- True
-- *Week2> nnf . arrowfree $ form1
-- +(*(+(-1 2) +(2 -1)) *(*(1 -2) *(-2 1)))
-- *Week2> equiv form1 (nnf . arrowfree $ form1)
-- True
-- *Week2> (cnf . nnf . arrowfree $ form1)
-- *(*(*(+(+(-1 2) 1) +(+(-1 2) -2)) *(+(+(-1 2) -2) +(+(-1 2) 1))) *(*(+(+(2 -1) 1) +(+(2 -1) -2)) *(+(+(2 -1) -2) +(+(2 -1) 1))))
-- *Week2> equiv form1 (cnf . nnf . arrowfree $ form1)
-- True

testCNF1 = all testCnf [form1, form2, form3]
testCNF2 = all testCnf (allTaut ++ allCont ++ allSatis)
testCNF3 = all testCnf [form1, form2, form3]
