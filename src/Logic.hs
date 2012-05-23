module Logic where

data Term = Const String [Term]
          | Var String

data TT a     = TT 
data FF a     = FF
data Atom a   = Atom String [Term]
data Not a    = Not a
data Or a     = Or a a
data And a    = And a a
data Impl a   = Impl a a
data Equiv a  = Equiv a a
data Exists a = Exists (Term -> a)
data Forall a = Forall (Term -> a)

