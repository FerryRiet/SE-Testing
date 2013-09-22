module LabWeek3 
where
import Data.List
import Data.Char
import System.Random
import Week3
import Techniques

{-
	Assignment: 3
	Time: 15 min
-}

genIntList :: IO [Int]
genIntList = do
				putStr "Enter number of length of list: " 
				ll <- getLine
				let lengthOfList = (read ll :: Int)
				generateLists lengthOfList
				
generateLists :: Int -> IO [Int]
generateLists 0 = return []
generateLists x = do 
					randomNr <-  getRandomInt x
					list <- generateLists (x-1)
					return (randomNr:list)
{-
	Assignment: 4
	Time: 5 minutes
	I first tried to just sort the elements and compare them but this is not working for all the types (Ord).
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] ys = False
isPermutation xs [] = False
isPermutation (x:xs) ys = isPermutation xs (delete x ys)
{-
	Assignment: 5
	Time: 45 min
-}

testPermutation1 xs 	= isPermutation xs xs
testPermutation2 xs ys 	= isPermutation xs ys

testProp :: Int -> IO ()
testProp n = do	
		if n <= 0 then 
			return ()
		else 
			do	
			    xs <- genIntList	
			    if (testPermutation1 xs) then
			        do
			            putStr ("Passed test #" ++ (show n) ++ "\n")
				    testProp (n - 1)
			    else 
				do
			    	    putStr ("Failed test #" ++ (show n) ++ "\n")
				    testProp (n - 1)
								
testProp2 :: Int -> IO ()
testProp2 n = do	
		if n <= 0 then 
	        	return ()
		else 
		    do	
			xs <- genIntList
			ys <- genIntList
			if (testPermutation2 xs ys) then 
				do
				    putStr ("Passed test #" ++ (show n) ++ "\n")
				    testProp2 (n - 1)
			else 
				do
				    putStr ("Failed test #" ++ (show n) ++ "\n")
		        	    testProp2 (n - 1)

{- 
	Assignment:7
	Time: 2,5 hour
-}

getRandomName :: Int -> IO [Char]
getRandomName 0 = return []
getRandomName n = do
                    c <- randomRIO('a', 'z')
                    cs <- getRandomName (n - 1)
                    return (c:cs)


getRandomTerm :: IO Term
getRandomTerm = do
        n <- getRandomInt 1
        case n of
                0 -> do
                        name <- getRandomName 3
                        return (V name)
                1 -> do
                        i <- getRandomInt 2
                        ts <- getRandomTerms i
                        name <- getRandomName 5
                        return (F name ts )

getRandomTerms :: Int -> IO [Term]
getRandomTerms 0 = return []
getRandomTerms n = do
                     t <- getRandomTerm
                     ts <- getRandomTerms (n - 1)
                     return (t:ts)

getRandomFormula :: IO Formula
getRandomFormula = do
                    n <- getRandomInt 8
                    case n of
                        0   ->  do
                                    name <- getRandomName 3
                                    t <- getRandomTerms 1
                                    return (Atom name t)
                        1   ->  do
                                    t1 <- getRandomTerm
                                    t2 <- getRandomTerm
                                    return (Eq t1 t2)
                        2   ->  do
                                    m <- getRandomInt 3
                                    f <- getRandomFormulas m
                                    return (Disj f)
                        3   ->  do
                                    m <- getRandomInt 3
                                    f <- getRandomFormulas m
                                    return (Conj f)
                        4   ->  do
                                    f <- getRandomFormula
                                    return (Neg f)
                        5   ->  do
                                    f1 <- getRandomFormula
                                    f2 <- getRandomFormula
                                    return (Impl f1 f2)
                        6   ->  do
                                    f1 <- getRandomFormula
                                    f2 <- getRandomFormula
                                    return (Equi f1 f2)
                        7   ->  do
                                    name <- getRandomName 1
                                    f <- getRandomFormula
                                    return (Forall name f)
                        8   ->  do
                                    name <- getRandomName 1
                                    f <- getRandomFormula
                                    return (Exists name f)


getRandomFormulas :: Int -> IO [Formula]
getRandomFormulas 0 = return []
getRandomFormulas n = do
                        f <- getRandomFormula
                        fs <- getRandomFormulas (n-1)
                        return (f:fs)
