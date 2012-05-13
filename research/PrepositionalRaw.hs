module PrepositionalRaw where

import Prelude hiding(not, or, and)
import Data.List(nub)
import Control.Monad.State

---
--- Predicate Type
---
data Predicate = F
    | Variable String
    | Not Predicate
    | And Predicate Predicate
    | Or  Predicate Predicate
    | If  Predicate Predicate
    | Iff Predicate Predicate
    deriving(Show, Read, Eq)

data Literal = Yes String
    | No String
    | TT
    | FF
    deriving(Show, Read, Eq)

newtype Clause = Clause [Literal] 
    deriving(Show, Read, Eq)

newtype CNFPredicate = CNFPredicate [Clause]
    deriving(Show, Read, Eq)

-- Improved logical functions
not :: Predicate -> Predicate
not (Not p)   = p
not p         = Not p

or :: Predicate -> Predicate -> Predicate
or F p        = p
or p F        = p
or _ (Not F)  = not F
or p (Not q)
    | p == q = not F
    | otherwise = Or p (Not q)
or (Not q) p
    | p == q = not F
    | otherwise = Or (Not q) p
or p q        = Or p q

and :: Predicate -> Predicate -> Predicate
and p F       = F
and F p       = F
and p (Not F) = p
and (Not F) p = p
and p (Not q)
    | p == q = F
    | otherwise = And p (Not q)
and (Not p) q
    | p == q = F
    | otherwise = And (Not p) q
and p q       = And p q

--- functions for CNF
-- Alternative Elimination
eliminateIf :: Predicate -> Predicate
eliminateIf F            = F
eliminateIf (Variable s) = Variable s
eliminateIf (Not p)      = not (eliminateIf p)
eliminateIf (And p q)    = and (eliminateIf p) (eliminateIf q)
eliminateIf (Or p q)     = or  (eliminateIf p) (eliminateIf q)
eliminateIf (If p q)     = 
    let p' = eliminateIf p
        q' = eliminateIf q
    in 
        or (not p') q'
eliminateIf (Iff p q)    = 
    let p' = eliminateIf p
        q' = eliminateIf q
    in
        or (and (not p') (not q')) (and p' q')
   
-- Moving Negation down the predicate tree
moveNegation :: Predicate -> Predicate
moveNegation (Not (Or p q))  = 
    let p' = moveNegation p
        q' = moveNegation q
    in
        and (not p') (not q') 
moveNegation (Not (And p q)) = let p' = moveNegation p
                                   q' = moveNegation q
                               in
                                   or (not p') (not q')
moveNegation (And p q)       = and (moveNegation p) (moveNegation q) 
moveNegation (Or p q)        = or  (moveNegation p) (moveNegation q) 
moveNegation (Not (Not p))   = p
moveNegation p               = p


-- Shifting alternative inside conjunction
shiftAlternative (Or (And p q) (And r s)) = 
    let a = shiftAlternative (or p r)
        b = shiftAlternative (or p s)
        c = shiftAlternative (or q r)
        d = shiftAlternative (or q s)
    in
        and (and a b) (and c d)
shiftAlternative (Or p (And q r)) = 
    let a = shiftAlternative (or p q)
        b = shiftAlternative (or p r)
    in
        and a b 
shiftAlternative (Or (And p q) r) = 
    let a = shiftAlternative (or p r)
        b = shiftAlternative (or q r)
    in
        and a b
shiftAlternative (And p q)        = and (shiftAlternative p) (shiftAlternative q)
shiftAlternative (Or p q)         = or  (shiftAlternative p) (shiftAlternative q)
shiftAlternative p                = p

cnfify :: Predicate -> Predicate
cnfify = shiftAlternative . moveNegation . eliminateIf

-- Conversion to CNFPredicate

toCNF :: Predicate -> CNFPredicate
toCNF p = optimisePredicate 
          $ CNFPredicate 
          $ map (optimiseClause . createClause) 
                (execState (getOrList p') []) 
    where p' = cnfify p

getOrList :: Predicate -> State [Predicate] ()
getOrList (And p q) = do 
    getOrList q
    getOrList p
getOrList (Not F) = return ()
getOrList o = do
    os <- get
    put (o:os)

getVariableList :: Predicate -> State [Predicate] ()
getVariableList (Or p q) = do
    getVariableList q
    getVariableList p
getVariableList F = return ()
getVariableList v = do
    vs <- get 
    put (v:vs)

toLiteral :: Predicate -> Literal
toLiteral (Variable x)       = Yes x
toLiteral (Not (Variable x)) = No  x
toLiteral F                  = FF
toLiteral (Not F)            = TT
toLiteral s                  = error ("Expected simple literal: " ++ show s)

createClause :: Predicate -> Clause
createClause p = Clause $ map toLiteral (execState (getVariableList p) [])

optimiseClause :: Clause -> Clause
optimiseClause (Clause lits) = (Clause . nub . filt . opt) lits
    where filt :: [Literal] -> [Literal]
          filt l = if TT `elem` l 
                     then [TT]
                     else filter (/=FF) l
          opt :: [Literal] -> [Literal]
          opt []           = []
          opt (TT:ls)      = [TT]
          opt (FF:ls)      = opt ls
          opt ((Yes x):ls) = if No x `elem` ls
                                then [TT]
                                else (Yes x) : opt ls
          opt ((No x):ls) = if Yes x `elem` ls
                                then [TT]
                                else (No x) : opt ls

optimisePredicate :: CNFPredicate -> CNFPredicate
optimisePredicate (CNFPredicate clauses) = 
    CNFPredicate $ nub $ filter (/= Clause [TT]) clauses


---
--- Joining predicates
---
joinCNF :: CNFPredicate -> CNFPredicate -> CNFPredicate
joinCNF (CNFPredicate p) (CNFPredicate q) = 
    optimisePredicate $ CNFPredicate $ p ++ q

addToCNF :: CNFPredicate -> Predicate -> CNFPredicate
addToCNF cnfP nonCnfq = joinCNF cnfP (toCNF nonCnfq)
