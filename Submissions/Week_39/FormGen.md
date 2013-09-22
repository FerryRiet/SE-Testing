## Haskell prelude

Implementing a random Formula generator based on the definition in the file *Week3.hs*.

Time spend 3.5 hours due to a small war between me, spaces, tabs and Haskell 

``` Haskell
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

```

From the definition

        type Name = String
        data Term = V Name 
                  | F Name [Term] 
                  deriving (Eq,Ord)

Add an character f in front of random name for readability.          

``` Haskell

getRandomTerm :: Int -> IO Term 
getRandomTerm 0 = do n <- getRandomName 3
                     return (V n)
getRandomTerm x = do n <- getRandomInt 1
                     case n of
                           0 -> do n <- getRandomName 3
                                   return (V n) 
                           1 -> do n <- getRandomName 3 
                                   ts <- (getRandomTerms 3 (x-1))
                                   return (F ("f" ++ n) (nub ts)) 

getRandomTerms :: Int -> Int -> IO [Term]
getRandomTerms _ 0 = return []
getRandomTerms d n = do 
                        f <- getRandomTerm (d-1)
                        fs <- getRandomTerms (d-1) (n-1) 
                        return (f:fs)


```
On Atoms just for readability a "P" in front

``` Haskell

getRandomFormula :: Int -> IO Formula 
getRandomFormula 0 = do m <- getRandomName 3
                        ts <- getRandomTerms 3 3 
                        return (Atom ("P" ++ m) (nub ts))

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
                                     return (Conj (nub fs))
                             3 -> do m  <- getRandomInt 5 
                                     fs <- getRandomFormulas (d-1) m
                                     return (Disj (nub fs))
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

```

### Running simple tests generating and manually checking output.

Generating formulas of different depth an lists of 1..3.

``` Haskell

formulas_1_1 = getRandomFormulas 1 1
formulas_2_1 = getRandomFormulas 2 1
formulas_3_1 = getRandomFormulas 3 1
formulas_1_2 = getRandomFormulas 1 2
formulas_1_3 = getRandomFormulas 1 3
formulas_2_2 = getRandomFormulas 2 2
formulas_2_3 = getRandomFormulas 2 3
formulas_3_2 = getRandomFormulas 2 3
formulas_3_3 = getRandomFormulas 3 3

```

### Generated formulas
        *FormGen> formulas_1_1
        [fc[a,fb]==b]
        *FormGen> formulas_1_2
        [(Pc[fd[fd[fa[c]]],fd,c]==>Pd[fc[fc[b]],fc,b]),(Pc[a,c]==>Pb[fd[a],fa,c])]
        *FormGen> formulas_1_3
        [conj[Pa[b,fc,c],Pb[fc[fc[c]],a,b],Pb[b,fd]],(Pc[b,c]<=>Pa[fa[a],fc,d]),(Pb[d,b]==>Pa[fa[a],fd,d])]
        *FormGen> formulas_2_1
        [fd[fb[a],fd]==d]
        *FormGen> formulas_3_1
        [(Pa[d,fc,a]==>(E b Pb[fb[c],d,b]==>fc[c,fa]==a))]
        *FormGen> formulas_3_3
        [(disj[E d Pa[fa[fb[c]],fd,d],E b Pb[fa[fb[c]],b,c]]==>A a E a Pb[a,fd,b]),A b ~(Pc[d,a,b]==>Pd[b,fc]),(disj[~Pc[fa[c],a],fd[d,fb]==fb[fa[c],fd],E a Pb[d,b,c],~Pa[fa[fb[fa[fc[c]]]],c],Pb[a,fa,a]]<=>conj[(Pd[d,fb,a]==>Pb[fd[c],fa,d]),fa[d,a]==a])]


## Final

To extract the Haskell code from this document use the script called `md2hs` , the 
readable form of this file is rendered by the github "md" engine.
