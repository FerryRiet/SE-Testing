
-- 1.3

dividesBy :: Integer -> Integer -> Bool
dividesBy n k = rem k n == 0 

ld :: Integer -> Integer
ld d = ldf 2 d

ldf :: Integer -> Integer -> Integer
ldf k n | dividesBy k n = k
	| k^2 >= n       = n
	| otherwise     = ldf (k+1) n

-- 1.4

-- By replacing k^2> n by k2>= n it will not change the 
-- working of the function becouse the == n will be matched by
-- the dividesby condition

-- 1.5
isPrime :: Integer -> Bool
isPrime n | n < 1  = error "not a positive number" 
	  | n == 1 = False
	  | otherwise = (ld n) == n

-- 1.6 
-- rem :: Integer -> Integer -> Integer

-- 1.9

maxOflist :: [Integer] -> Integer
maxOflist []  = error "empty list"
maxOflist [x] = x
maxOflist (x:xs) = max x (maxOflist xs)

-- 1.10

removeFst :: Integer -> [Integer] -> [Integer]
removeFst m [] = error "empty list"
removeFst m [x] | m == x = [] 
		        | otherwise = [x] 
removeFst m (x:xs)  | m == x = xs
					| otherwise = x : removeFst m xs

srtInts :: [Integer] -> [Integer]
srtInts [] = [] 
srtInts xs = m : (srtInts (removeFst m xs)) where m = maxOflist xs

-- 1.13

count :: Char -> String -> Integer
count c [] = 0
count c [x] | c == x = 1 
			| otherwise = 0
count c (x:xs) | c == x = 1 + count c xs
			   | otherwise  = count c xs 

-- 1.14

mulChar :: Integer -> Char -> String
mulChar co x  | co == 1  = [x] 
              | otherwise = [x] ++ mulChar (co-1) x

blowupStart :: Integer -> String -> String
blowupStart c [x] = mulChar c x 
blowupStart c (x:xs) = mulChar c x ++ blowupStart (c+1) xs

blowup :: String -> String
blowup x = blowupStart 1 x

-- 1.15

-- Smaller not used
smaller :: String -> String -> Bool
smaller s1 s2 | len1 /= len2         = (len1 < len2)
              | otherwise            = (s1 < s2)
              where (len1, len2) = (length s1, length s2)

smallest :: String -> String -> String
smallest s1 s2 | s1 < s2 = s1
			   | otherwise = s2 


minStr :: [String] -> String
minStr []  = error "empty list"
minStr [x] = x
minStr (x:xs) = smallest x (minStr xs)

removeFstStr :: String -> [String] -> [String]
removeFstStr m [] = error "empty list"
removeFstStr m [x] | m == x = [] 
		           | otherwise = [x] 
removeFstStr m (x:xs)  | m == x = xs
					| otherwise = x : removeFstStr m xs

srtStrs :: [String] -> [String]
srtStrs [] = [] 
srtStrs xs = m : (srtStrs (removeFstStr m xs)) where m = minStr xs

--1.17

prefix :: String -> String ->Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

subString :: String -> String -> Bool
subString x [] =   False
subString x (y:ys) | prefix x (y:ys) = True
				   | otherwise = subString x ys












