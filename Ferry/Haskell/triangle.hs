-- isTriangle :: [int] -> bool

data Shape = NoTriangle 
		   | Equilateral
           | Isosceles 
           | Rectangular 
           | Other 
           deriving (Eq,Show)


isTriangle :: Integer -> Integer -> Integer -> Shape

isTriangle a b c  |  a == b && a == c  = Equilateral
				  |  a == c || b == c  || a == b = Isosceles
				  |  (a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2) = Rectangular 
                  |  (a + b > c || b + c > a || c + a > b) =  Other
                  |  otherwise = NoTriangle
