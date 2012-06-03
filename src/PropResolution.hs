{-# LANGUAGE FlexibleInstances #-}

module PropResolution where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.List(subsequences)
import Data.Char(chr)
import System.Random

import Carte
import Render
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

propEquiv :: Formula Input -> Bool
propEquiv f = let
    cf = (cnf . pushNeg . elimImpl) f
 in
    bruteSAT cf == satisfiable cf

-- quickCheck requires show 
instance Show (Formula Input) where
    show = pretty

instance Arbitrary (Formula Input) where
    arbitrary = MkGen randomF
  
randOpNo :: StdGen -> Bool -> Int
randOpNo rnd flat = let 
    bound = if flat then 2 else 7
 in
    fst $ randomR (0, bound) rnd

randProp :: StdGen -> String
randProp rnd = (chr (fst $ randomR (97, 122) rnd)) : []

randomF :: StdGen -> Int -> (Formula Input)
randomF gen n =
    let
        op = randOpNo gen (n <= 0)
        (gen1, gen2) = split gen
     in
        case op of
            0 -> L.tt
            1 -> L.ff
            2 -> prop $ randProp gen
            3 -> L.not $ randomF gen (n - 1)
            4 -> (randomF gen1 (n - 1)) `L.or`    (randomF gen2 (n - 1))
            5 -> (randomF gen1 (n - 1)) `L.and`   (randomF gen2 (n - 1))
            6 -> (randomF gen1 (n - 1)) `L.impl`  (randomF gen2 (n - 1))
            7 -> (randomF gen1 (n - 2)) `L.equiv` (randomF gen2 (n - 2))

--quickCheckWith stdArgs {maxSize = 9, maxSuccess = 1000} propEquiv 
