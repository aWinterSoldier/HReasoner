{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}


module Evaluate where


{-- Types --}

-- Expression's recursive type
data Expr f = In (f (Expr f))

-- simple value
data Val e = Val Int
type IntExpr = Expr Val

-- addition
data Add e = Add e e
type AddExpr = Expr Add

-- expression coproduct
infixr :+:
data (f :+: g) e = Inl (f e) | Inr (g e)


{-- Evaluation --}

-- operation folding
foldExpr :: (Functor f) => (f a -> a) -> Expr f -> a
foldExpr algebra (In t) = algebra (fmap (foldExpr algebra) t)

instance Functor Val where
    fmap f (Val x) = Val x

instance Functor Add where
    fmap f (Add x y) = Add (f x) (f y)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)

-- Evaluator class : operators are classes!
class (Functor f) => Eval f where
    evalAlgebra :: f Int -> Int

instance Eval Val where
    evalAlgebra (Val x) = x

instance Eval Add where
    evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr y) = evalAlgebra y

-- evaluation function
evaluate :: (Eval f) => Expr f -> Int
evaluate = foldExpr evalAlgebra



{-- Facilitating injections --}
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

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj 

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 |+|
(|+|) :: (Add :<: f) => Expr f -> Expr f -> Expr f
e1 |+| e2 = inject (Add e1 e2)


{-- Adding new operations --}

-- multiplication
data Mul e = Mul e e

instance Functor Mul where
    fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
    evalAlgebra (Mul x y) = x * y

infixl 7 |*|  -- important !
x |*| y = inject (Mul x y)

-- exponentiation
data Pow e = Pow e e

instance Functor Pow where
    fmap f (Pow x y) = Pow (f x) (f y)

instance Eval Pow where
    evalAlgebra (Pow x y) = x ^ y :: Int

infixr 8 |**|
x |**| y = inject (Pow x y)

type Arithmetic = Expr (Val :+: Add :+: Mul :+: Pow)

{-- Rendering --}
