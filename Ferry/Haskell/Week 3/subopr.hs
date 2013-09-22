
data Term = V String | F String [Term] deriving (Eq,Ord,Show)


x,y,z :: Term
x = V "x"
y = V "y"
z = V "z"


subst :: String -> Term -> Term -> Term
subst instr t (V  str) = if str == instr 
	                   then t 
	                   else (V str)
subst instr t (F str fs) = F str (map (subst instr t) fs)	                   


