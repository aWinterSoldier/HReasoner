{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module KnowledgeBase
(
    KnowledgeBase,
    consistent,
    learn,
    prove,
    runKB,
    evalKB,
    execKB
)
where

import Control.Monad.State

import Carte
import Propositional
import qualified LogicOperators as L
import Resolve

-- Knowledge Base type
newtype KnowledgeBase a = KB (State CNF a) deriving(Functor, 
    Monad, 
    MonadState CNF)

unwrapKB :: KnowledgeBase a -> State CNF a
unwrapKB (KB s) = s


{-- Knowledge Base manipulation functions --}
consistent :: KnowledgeBase Bool
consistent = do
    kb <- get
    return $ satisfiable kb

learn :: Formula Input -> KnowledgeBase ()
learn f = do
    kb <- get 
    put $ rectify (kb ++ convertToCNF f)

prove :: Formula Input -> KnowledgeBase Bool
prove f = do
    kb <- get 
    let 
        notF =  convertToCNF (L.not f)
    return $ (not . satisfiable) (kb ++ notF)

runKB :: KnowledgeBase a -> CNF -> (a, CNF)
runKB (KB s) f = runState s f

evalKB :: KnowledgeBase a -> CNF -> a
evalKB (KB s) f = evalState s f

execKB :: KnowledgeBase a -> CNF -> CNF
execKB (KB s) f = execState s f
