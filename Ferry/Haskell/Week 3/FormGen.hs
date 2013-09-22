module FormGen

where
  
import Week3
import Techniques 
import Data.List 
import Data.Char
import Logic

getRandomName :: Int -> IO Name 
getRandomName x = do
                     nn <- getRandomInt x 
                     return ([chr (97 + nn)])

getRandomTerm :: Int -> IO Term 
getRandomTerm 0 = do n <- getRandomName 3
                     return (V n)
getRandomTerm x = do n <- getRandomInt 1
                     case n of
                           0 -> do n <- getRandomName 3
                                   return (V n) 
                           1 -> do n <- getRandomName 3 
                                   ts <- (getRandomTerms 3 (x-1))
                                   return (F ("f" ++ n) ts) 

getRandomTerms :: Int -> Int -> IO [Term]
getRandomTerms _ 0 = return []
getRandomTerms d n = do 
                        f <- getRandomTerm (d-1)
                        fs <- getRandomTerms (d-1) (n-1) 
                        return (f:fs)

getRandomFormula :: Int -> IO Formula 
getRandomFormula 0 = do m <- getRandomName 3
                        ts <- getRandomTerms 3 3 
                        return (Atom ("P" ++ m) ts)

getRandomFormula d = do n <- getRandomInt 8
                        case n of 
                             0 -> do m <- getRandomInt 20
                                     nn <- getRandomTerms 3 3 
                                     name <- getRandomName 3
                                     return (Atom ("P" ++ name) nn)
                             1 -> do f <- getRandomFormula (d-1)
                                     return (Neg f) 
                             2 -> do m  <- getRandomInt 5 
                                     fs <- getRandomFormulas (d-1) m
                                     return (Conj fs)
                             3 -> do m  <- getRandomInt 5 
                                     fs <- getRandomFormulas (d-1) m
                                     return (Disj fs)
                             4 -> do f <- getRandomFormula (d-1)
                                     g <- getRandomFormula (d-1)
                                     return (Impl f g) 
                             5 -> do f <- getRandomFormula (d-1)
                                     g <- getRandomFormula (d-1)
                                     return (Equi f g) 
                             6 -> do f <- getRandomName 3
                                     g <- getRandomFormula (d-1)
                                     return (Forall f g) 
                             7 -> do f <- getRandomName 3
                                     g <- getRandomFormula (d-1)
                                     return (Exists f g) 
                             8 -> do f <- getRandomTerm 3
                                     g <- getRandomTerm 3
                                     return (Eq f g) 

getRandomFormulas :: Int -> Int -> IO [Formula]
getRandomFormulas _ 0 = return []
getRandomFormulas d n = do 
                        f <- getRandomFormula d
                        fs <- getRandomFormulas d (n-1) 
                        return (f:fs)

