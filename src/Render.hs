module Render where

import Carte(Formula (In))
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
    render (Or x y) = "(" ++ pretty y ++ " or " ++ pretty x ++ ")"

instance Render And where
    render (And x y) = "(" ++ pretty y ++ " and " ++ pretty x ++ ")"
    
instance Render Impl where
    render (Impl x y) = "(" ++ pretty y ++ " => " ++ pretty x ++ ")"

instance Render Equiv where
    render (Equiv x y) = "(" ++ pretty y ++ " <=> " ++ pretty x ++ ")"
