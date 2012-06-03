{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Propositional where

import Prelude hiding(and, or, not)
import Data.List.Utils(join)

import Carte
import LogicOperators
import Render

{-- Prepositional logic constructs --}
data Prop    a = Prop String
data NotProp a = NotProp String

instance Functor Prop where
    fmap _ (Prop s) = Prop s

instance Render Prop where
    render (Prop s) = s

prop :: (Prop :<: f) => String -> Formula f
prop s = inject $ Prop s

instance Functor NotProp where
    fmap _ (NotProp s) = NotProp s

instance Render NotProp where
    render (NotProp s) = "~" ++ s

notProp :: (NotProp :<: f) => String -> Formula f
notProp s = inject $ NotProp s

{-- CNF Conversion --}

-- input formula
type Input = TT :+: FF :+: Prop :+: Not :+: Or :+: And :+: Impl :+: Equiv

-- implication and equivalence elimination
type Stage1 = TT :+: FF :+: Prop :+: Not :+: Or :+: And

elimImpl :: Formula Input -> Formula Stage1
elimImpl = foldFormula elimImplAlg


-- TODO: Think about eliminating boilerplate instances
class (Functor f) => ElimImpl f where
    elimImplAlg :: f (Formula Stage1) -> Formula Stage1

instance ElimImpl TT where
    elimImplAlg TT = tt

instance ElimImpl FF where
    elimImplAlg FF = ff

instance ElimImpl Prop where
    elimImplAlg (Prop s) = prop s

instance ElimImpl Not where 
    elimImplAlg (Not x) = not x

instance ElimImpl Or where
    elimImplAlg (Or x y) = or x y

instance ElimImpl And where
    elimImplAlg (And x y) = and x y

instance ElimImpl Impl where
    elimImplAlg (Impl x y) = or (not x) y

instance ElimImpl Equiv where 
    elimImplAlg (Equiv x y) = and (or (not x) y) (or (not y) x)

instance (ElimImpl f, ElimImpl g) => ElimImpl (f :+: g) where
    elimImplAlg (Inl x) = elimImplAlg x
    elimImplAlg (Inr x) = elimImplAlg x

-- negation pushdown
type Stage2 = TT :+: FF :+: Prop :+: NotProp :+: Or :+: And

pushNeg :: Formula Stage1 -> Formula Stage2
pushNeg = foldFormula pushNegAlg

dualise :: Formula Stage2 -> Formula Stage2
dualise = foldFormula dualAlg

class (Functor f) => Dualise f where
    dualAlg :: f (Formula Stage2) -> Formula Stage2

instance Dualise TT where
    dualAlg TT = ff

instance Dualise FF where
    dualAlg FF = tt

instance Dualise Prop where
    dualAlg (Prop s) = notProp s

instance Dualise NotProp where
    dualAlg (NotProp s) = prop s

instance Dualise Or where
    dualAlg (Or x y) = and x y

instance Dualise And where
    dualAlg (And x y) = or x y

instance (Dualise f, Dualise g) => Dualise (f :+: g) where
    dualAlg (Inl x) = dualAlg x
    dualAlg (Inr x) = dualAlg x


class (Functor f) => PushNeg f where
    pushNegAlg :: f (Formula Stage2) -> Formula Stage2

instance PushNeg TT where
    pushNegAlg TT = tt

instance PushNeg FF where
    pushNegAlg FF = ff

instance PushNeg Prop where
    pushNegAlg (Prop s) = prop s

instance PushNeg Not where
    pushNegAlg (Not x) = dualise x

instance PushNeg Or where
    pushNegAlg (Or x y) = or x y

instance PushNeg And where
    pushNegAlg (And x y) = and x y

instance (PushNeg f, PushNeg g) => PushNeg (f :+: g) where
    pushNegAlg (Inl x) = pushNegAlg x
    pushNegAlg (Inr x) = pushNegAlg x

-- distributing alternative over conjunction
type Literal = Prop :+: NotProp
type Clause  = [Formula Literal]
type CNF     = [Clause]

instance Eq (Formula Literal) where
    (In (Inl (Prop s))) == (In (Inl (Prop p))) = p == s
    (In (Inr (NotProp s))) == (In (Inr (NotProp p))) = p == s
    _ == _ = False

instance Ord (Formula Literal) where
    (In (Inl (Prop _))) <= (In (Inr (NotProp _))) = True
    (In (Inl (Prop s))) <= (In (Inl (Prop q))) = s <= q
    (In (Inr (NotProp s))) <= (In (Inr (NotProp q))) = s <= q

literalSymbol :: Formula Literal -> String
literalSymbol (In (Inr (NotProp s))) = s
literalSymbol (In (Inl (Prop s))) = s

literalSign :: Formula Literal -> Bool
literalSign (In (Inr _)) = False
literalSign (In (Inl _)) = True

prettyCNF :: CNF -> String
prettyCNF c = join "/\\" (map (bracket . prettyClause) c)
    where bracket s = "(" ++ s ++ ")"

prettyClause :: Clause -> String
prettyClause c = join "\\/" (map pretty c)

(\/) :: Clause -> Clause -> Clause
(\/) = (++)

(/\) :: CNF -> CNF -> CNF
(/\) = (++)

cnf :: Formula Stage2 -> CNF
cnf = foldFormula cnfAlg

convertToCNF :: Formula Input -> CNF
convertToCNF = cnf . pushNeg . elimImpl

class (Functor f) => ToCNF f where
    cnfAlg :: f CNF -> CNF

instance ToCNF TT where 
    cnfAlg TT = []

instance ToCNF FF where
    cnfAlg FF = [[]]

instance ToCNF Prop where
    cnfAlg (Prop s) = [[inject (Prop s)]]

instance ToCNF NotProp where
    cnfAlg (NotProp s) = [[inject (NotProp s)]]

instance ToCNF And where
    cnfAlg (And x y) = x /\ y

instance ToCNF Or where
    cnfAlg (Or x y) = [a \/ b | a <- x, b <- y]

instance (ToCNF f, ToCNF g) => ToCNF (f :+: g) where
    cnfAlg (Inl x) = cnfAlg x
    cnfAlg (Inr x) = cnfAlg x

{-- Conversion to Implicative Normal Form --}
data IClause = IClause 
                [Formula Prop] -- conjunction
                [Formula Prop] -- disjuntion
type INF = [IClause]

prettyIClause :: IClause -> String
prettyIClause (IClause con dis) = "(" ++ conStr ++ ")" ++ "=>" ++ "(" ++ disStr ++ ")"
    where conStr = join "/\\" (map pretty con)
          disStr = join "\\/" (map pretty dis)

prettyINF :: INF -> String
prettyINF f = join "\n" (map prettyIClause f)

inf :: CNF -> INF
inf = map toImpl 
    where toImpl disj = IClause [prop s | In (Inr (NotProp s)) <- disj]
                                [prop s | In (Inl (Prop s))  <- disj]
