module Render where

import Carte(
             Formula (In), 
             (:+:) (Inl, Inr)
            )
import LogicOperators

class (Functor f) => Render f where
    render :: Render g => f (Formula g) -> String

pretty :: Render f => Formula f -> String
pretty (In x) = render x

instance Render TT where
    render TT = "true"

instance Render FF where
    render FF = "false"

instance Render Not where
    render (Not x) = "~(" ++ pretty x ++ ")"

instance Render Or where
    render (Or x y) = "(" ++ pretty x ++ " or " ++ pretty y ++ ")"

instance Render And where
    render (And x y) = "(" ++ pretty x ++ " and " ++ pretty y ++ ")"
    
instance Render Impl where
    render (Impl x y) = "(" ++ pretty x ++ " => " ++ pretty y ++ ")"

instance Render Equiv where
    render (Equiv x y) = "(" ++ pretty x ++ " <=> " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
    render (Inl x) = render x
    render (Inr x) = render x
