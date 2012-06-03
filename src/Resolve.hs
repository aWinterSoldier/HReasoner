module Resolve
--(
--  satisfiable,
--  rectify
--)
where

import qualified Data.Set as S
import Data.List(nub)

import Propositional 

containsLiteral :: Clause -> String -> Maybe Bool
containsLiteral []     _ = Nothing
containsLiteral (c:cs) s
    | literalSymbol c == s = Just $ literalSign c
    | otherwise            = containsLiteral cs s

-- extracts valid proposition names from a CNF formula
propList :: CNF -> [String]
propList f = S.toList (foldl insertClause S.empty f)
    where insertClause :: S.Set String -> Clause -> S.Set String
          insertClause s c = foldl (flip S.insert) s (map literalSymbol c)

-- finds clauses with a positive and negative appearance of given literal
findClauses :: CNF -> String -> ([Clause], [Clause])
findClauses []     l = ([], [])
findClauses (c:cs) l = let 
        (ps, ns) = findClauses cs l
    in 
        case containsLiteral c l of
            Nothing    -> (ps, ns)
            Just True  -> (c:ps, ns)
            Just False -> (ps, c:ns)

-- combines clauses with positive and negative appearances of literal
combine :: [Clause] -> [Clause] -> String -> [Clause]
combine pos neg l = do
    let 
        isNotPos x = (not (literalSign x)) || literalSymbol x /= l
        isNotNeg x = literalSign x || literalSymbol x /= l
        pos' :: [Clause]
        pos' = map (filter isNotPos) pos
        neg' = map (filter isNotNeg) neg
    p <- pos'
    n <- neg'
    return (p ++ n)
    
eliminateDuplicates :: [Clause] -> [Clause]
eliminateDuplicates = map nub

eliminateTrueClauses :: [Clause] -> [Clause]
eliminateTrueClauses = filter noDual
    where noDual :: Clause -> Bool
          noDual c = let
              ps = S.fromList [literalSymbol l | l <- c, literalSign l]
              ns = S.fromList [literalSymbol l | l <- c, not $ literalSign l]
           in
              S.null (ps `S.intersection` ns)

rectify :: [Clause] -> [Clause]
rectify = nub . eliminateDuplicates . eliminateTrueClauses

resolutionStep :: CNF -> [String] -> CNF
resolutionStep f ls = resolutionStep' f ls []

resolutionStep' :: CNF -> [String] -> CNF -> CNF
resolutionStep' _ []     acc = acc
resolutionStep' f (l:ls) acc = let
    (p, n)     = findClauses f l
    newClauses = combine p n l
    newAcc     = rectify (newClauses ++ acc)
 in 
    resolutionStep' f ls newAcc

satisfiable :: CNF -> Bool
satisfiable f = satisfiable' f (propList f)
    where satisfiable' f lits = let
             newClauses = resolutionStep f lits
           in
             case newClauses of
               [] -> True
               l | [] `elem` l -> False
               l | otherwise   -> satisfiable' (f ++ l) lits
