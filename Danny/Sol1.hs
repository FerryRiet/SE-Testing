module Sol1
where
import GS

-- Exercise 1.9
mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

-- Exercise 1.10
--removeFst :: Int -> [Int] -> [Int] -- This one I had before exercise 1.15
removeFst :: Eq a => a -> [a] -> [a] -- Migrated to this declaration for ex 1.15
removeFst m [] = []
removeFst m (x:ms) | m == x = ms
                   | otherwise = x : (removeFst m ms)

-- Exercise 1.13
count :: Char -> String -> Int
count chr [] = 0
count chr (s:strng) | chr == s  = 1 + (count chr strng)
                    | otherwise = count chr strng

-- Exercise 1.14
blowup :: String -> String
blowup "" = ""
blowup strng = blowup2 strng 1

blowup2 :: String -> Int -> String
blowup2 [] n = []
blowup2 (s:strng) n = (multiplyChar s n) ++ (blowup2 strng (n + 1))

multiplyChar :: Char -> Int -> String
multiplyChar chr 0 = []
multiplyChar chr n = chr: multiplyChar chr (n-1)

-- Exercise 1.15
srtString :: [String] -> [String]
srtString [] = []
srtString str = min : srtString (removeFst min str) where min = mnmStr str

mnmStr :: [String] -> String
mnmStr [] = []
mnmStr [x] = x
mnmStr (x:xs) = min x (mnmStr xs)

-- Exercise 1.17
subString :: String -> String -> Bool
subString [] str2 = True
subString str1 [] = False
subString (s1:str1) (s2:str2) | prefix (s1:str1) (s2:str2) = True
                              | subString (s1:str1) str2 = True
                              | otherwise = False

-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths xs = map length xs

-- Exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)
