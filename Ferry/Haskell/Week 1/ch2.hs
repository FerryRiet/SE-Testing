-- CH2 (TAMO)

module TAMO
where

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
True ==> x = x



False ==> x = True

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

--    
-- exclusive or
--       P       Q      <+>
--       f       f      f 
--       f       t      t 
--       t       f      t 
--       f       f      f 

infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

-- /= op ???  not equal

p = True
q = False
formula1 = (not p) && (p ==> q) <=> not (q && (not p))

formula2 p q = ((not p) && (p ==> q) <=> not (q && (not p)))
