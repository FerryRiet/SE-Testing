module LabAssign6
where

import Week6
import Data.List
import System.Random

{-
    Assignment: 1 & 2

    if b is even and b > 0  -->     (a2)(b/2)
    if b is odd             -->     a^b = a*(a^2)^((b-1)/2)
    if b = 0                -->     1

    Takes time: O( log(b) )
-}
exM' :: Integer -> Integer -> Integer -> Integer
exM' a b c = rem (ex a b) c

ex :: Integer -> Integer -> Integer
ex a 0 = 1
ex a b = if (rem b 2 == 0) then
            (a^2)^ quot b 2
         else
            ((a^2) ^ quot (b-1) 2) * a

-- Test if results of own implementation are valid
-- Assuming results from expM are correct!
testValid = and [ (exM' a b c)  == (expM a b c) | a <- [2,4..100], b <- [1..50], c <- [1..75]]

-- ToDo: test if impl is more efficient than expM
