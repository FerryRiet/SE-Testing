{---------------------------------------------------------

  Jan van Eijck, October 2010, modified October 2012

 --------------------------------------------------------}

module RandomSudoku where 

import Data.List
import SudokuNRC
import System.Random

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- pick a random member from a list
-- empty list indicates failure
getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = 
  do n <- getRandomInt maxi
     return [xs !! n]
     where maxi = length xs - 1

-- randomize a list
randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)                     

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs 
                        then return []
                        else return (extendNode (s,cs\\xs) (head xs))

-- find a random solution for a member of 
rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else rsearch succ goal (return $ tail xs)
          
genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False


-- erase a position from a Sudoku
eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

-- erase a position from a Node 
eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

-- return a "minimal" node with a unique solution
-- by erasing positions until the result becomes ambiguous
minimalize :: Node -> [(Row,Column)] -> Int -> Node
minimalize n [] _ = n
minimalize n ((r,c):rcs) d | classifyConstraints (snd n) >= d = n 
                           | uniqueSol n' = minimalize n' rcs d
                           | otherwise    = minimalize n  rcs d
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> Int -> IO Node
genProblem n l = do ys <- randomize xs
                    return (minimalize n ys l)
               where xs = filledPositions (fst n)


minimalize' :: Node -> [(Row,Column)] -> Node
minimalize' n [] = n
minimalize' n ((r,c):rcs)  | uniqueSol n' = n'
                           | otherwise    = minimalize' n  rcs
  where n' = eraseN n (r,c)

isMinimal :: Node -> Bool
isMinimal n = if sud2grid (fst n) == sud2grid (fst (minimalize' n xs)) 
              then True
              else False
              where xs = filledPositions (fst n)

getRandomSudokus :: Int -> Int -> IO [Node]
getRandomSudokus 0 _ = return [] 
getRandomSudokus n level = do s <- genRandomSudoku
                              --d <- level 
                              l <- genProblem s level
                              l2 <- getRandomSudokus (n-1) level
                              return (l:l2)

-- Test drivers

testSudoku :: Int -> (Node  -> Bool) -> [Node] -> IO ()
testSudoku n _ []  = print (show n ++ " Sudoku tests passed")
testSudoku n p (f:fs) = 
                      if p f
                      then do print ("Pass on:")
                              showNode f
                              testSudoku n p fs
                      else do showNode f
                              error ("test failed.")

testSudokus :: Int -> Int -> (Node -> Bool) -> IO ()
testSudokus n d prop = do 
                      fs1 <- getRandomSudokus n d
                      testSudoku n prop fs1 

{-
  testSudodus will generate Sudokus of given difficulty and runs a test on it.
-}

tSud0 = testSudokus 3 40 (\ x ->  consistent (fst x) )                      
tSud1 = testSudokus 3 40 (\ x ->  consistent (fst (solveNs [x]!! 0)) )                      
tSud2 = testSudokus 3 40 (\ x ->  length (filledPositions (fst (solveNs [x]!! 0))) == 81 )                      
tSud3 = testSudokus 3 40 (\ x ->  isMinimal x == False )                      
tSud4 = testSudokus 1 400 isMinimal
--tSud5 = testSudokus 1 40 (\ x -> show x )                      



--main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r 100
          showNode s
          solveShowNs [s]

--pain :: IO ()
pain = do r <- genRandomSudoku 
          showNode r
          s  <- genProblem r 350
          showNode s
          --showConstraints $ snd s
          if isMinimal s 
          then print "Minimal" 
          else print "Not-minimal"
          print ("Class of Sudoku: " ++ show (classifyConstraints (snd s)))
          solveShowNs [s]



