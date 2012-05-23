{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Carte where

{-- Types --}

-- Recursive type for formula
data Formula f = In {out :: f (Formula f)}

-- Formula coproduct
infix 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl x) = Inl (fmap f x)
    fmap f (Inr x) = Inr (fmap f x)

foldFormula :: (Functor f) => (f a -> a) -> Formula f -> a
foldFormula algebra (In t) = algebra (fmap (foldFormula algebra) t)

{-- Injections --}
infixr :<:

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance Functor f => f :<: f where
    inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj
-- The above is not complete, but it suffices!

inject :: (g :<: f) => g (Formula f) -> Formula f
inject = In . inj 


