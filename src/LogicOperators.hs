{-# LANGUAGE FlexibleContexts #-}

module LogicOperators where

import Carte(
             Formula, 
             (:<:),
             inject
            )

data TT a     = TT 
data FF a     = FF
data Not a    = Not a
data Or a     = Or a a
data And a    = And a a
data Impl a   = Impl a a
data Equiv a  = Equiv a a

{-- Functor instances --}
instance Functor TT where
    fmap _ _ = TT

instance Functor FF where
    fmap _ _ = FF

instance Functor Not where 
    fmap f (Not x) = Not (f x)

instance Functor Or where
    fmap f (Or x y) = Or (f x) (f y)

instance Functor And where
    fmap f (And x y) = And (f x) (f y)

instance Functor Impl where
    fmap f (Impl x y) = Impl (f x) (f y)

instance Functor Equiv where
    fmap f (Equiv x y) = Equiv (f x) (f y)

{-- Smart Constructors --}
tt :: (TT :<: f) => Formula f
tt = inject TT

ff :: (FF :<: f) => Formula f
ff = inject FF

not :: (Not :<: f) => Formula f -> Formula f
not = inject . Not

or :: (Or :<: f) => Formula f -> Formula f -> Formula f
or x y = inject $ Or x y

and :: (And :<: f) => Formula f -> Formula f -> Formula f
and x y = inject $ And x y

impl :: (Impl :<: f) => Formula f -> Formula f -> Formula f
impl x y = inject $ Impl x y

equiv :: (Equiv :<: f) => Formula f -> Formula f -> Formula f
equiv x y = inject $ Equiv x y
