{-# LANGUAGE FlexibleContexts #-}

module Propositional where

import Prelude hiding(and, or, not)
import Carte
import LogicOperators

{-- Prepositional logic constructs --}
data Prop    a = Prop String
data NotProp a = NotProp String

instance Functor Prop where
    fmap _ (Prop s) = Prop s

prop :: (Prop :<: f) => String -> Formula f
prop s = inject $ Prop s

instance Functor NotProp where
    fmap _ (NotProp s) = NotProp s

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




