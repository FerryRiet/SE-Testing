import Week2
import Logic

--------------------------
cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree -- Don't listen to Jimmy 

cnf' :: Form -> Form
cnf' (Prop x) = Prop x
cnf' (Neg  x) = Neg x        -- Do listem to Jimmy
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


t1 = testCNF (Dsj [Dsj [p,q],q])


--nodist :: (Form,Form) -> Form
--nodist Cnj f j   = j
--nodisk f Cnj j  = f
--nodisk f j       = Dsj f j