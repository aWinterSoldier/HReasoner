module PropResolution where

import Test.QuickCheck
import Data.List(subsequences)

import Carte
import qualified LogicOperators as L
import Propositional
import Resolve

bruteSAT :: CNF -> Bool
bruteSAT f = let
    lits = propList f
    trueLits = subsequences lits
 in
    bruteSAT' f trueLits

bruteSAT' :: CNF -> [[String]] -> Bool
bruteSAT' _ []     = False
bruteSAT' f (l:ls) = if evaluate f l
                        then True
                        else bruteSAT' f ls

evaluate :: CNF -> [String] -> Bool
evaluate f ls = all hasTrueLit f
    where hasTrueLit :: Clause -> Bool
          hasTrueLit = any (smartElem ls)
          smartElem :: [String] -> Formula Literal -> Bool
          smartElem ls (In (Inl (Prop s))) = elem s ls
          smartElem ls (In (Inr (NotProp s))) = not (elem s ls)
          -- TODO: prettify


propEquiv :: CNF -> Bool
propEquiv f = let
    cf = (cnf . pushNeg . elimImpl) f
 in
    bruteSAT cf == satisfiable cf
