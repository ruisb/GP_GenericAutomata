{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE FlexibleContexts#-}
module FAutomata.Operations where

import FAutomata.Representation

import ParRegular.Representations
import ParRegular.Base
import ParRegular.EqFunctor

import ParRegular.Data.ESet
import ParRegular.Data.FiniteFunctions

import Control.Monad 
import Control.Arrow ((***))
import Data.List (nub)
import Control.Monad.Reader


----------------------------------------------
-- Runs
----------------------------------------------

-- | The common pattern for the run functions

runPatt :: (Ord a,Functor f) => (s -> res, f res -> res) -> LTS f a s -> [a] -> s -> res
runPatt (eta,mu) delta []      = eta
runPatt (eta,mu) delta (a:as)  = mu . fmap (runPatt (eta,mu) delta as) . (delta \$ a)


-- | Forgetful Run (reduce to sets)
runForgetful :: (Ord a, Eq s, Crush f, Functor f) =>  LTS f a s -> [a] -> s -> ESet s
runForgetful = runPatt (singleton, setjoin . toSet)

toSet :: (Eq a, Crush f) => f a -> ESet a
toSet = crush Crusher{plus = union, zero = empty, inj = singleton}


-- | Finite power of a functor
data Star f x = End x | Step (f (Star f x)) 

instance (Show1 f, Show x) => Show (Star f x) where
  show (End x)    = "End " ++ show x
  show (Step fx)  = "Step " ++ show1 fx 

class Show1 f where
  show1 :: (Show x) => f x -> String

-- | All-Structure Preserving Run (using the Star type)
runPreserving :: (Ord a, Functor f) => LTS f a s -> [a] -> s -> Star f s
runPreserving = runPatt (End,Step)

-- | Run In a Monadic Structure
runInMonad :: (Ord a, Monad f, Functor f) =>  LTS f a s -> [a] -> s -> f s
runInMonad = runPatt (return,join)



----------------------------------------------
-- Runs for EFunctors
----------------------------------------------

erunPatt :: (Ord a, EFunctor f, Eq s, Eq res) => (s -> res, f res -> res) -> LTS f a s -> [a] -> s -> res
erunPatt (eta,mu) delta []      = eta
erunPatt (eta,mu) delta (a:as)  = mu . emap (erunPatt (eta,mu) delta as) . (delta \$ a)


erunForgetful :: (Ord a, Eq s, Crush f, EFunctor f) =>  LTS f a s -> [a] -> s -> ESet s
erunForgetful = erunPatt (singleton, setjoin . toSet)

erunPreserving :: (Ord a, EFunctor f, Eq s, Eq1 (Star f)) => LTS f a s -> [a] -> s -> Star f s
erunPreserving = erunPatt (End,Step)

erunInMonad :: (Ord a, EMonad f, EFunctor f, Eq s, Eq1 f) =>  LTS f a s -> [a] -> s -> f s
erunInMonad = erunPatt (ereturn,ejoin)





----------------------------------------------
-- Product of Automata
----------------------------------------------


-- | Left strength of a functor
lstrength :: (Functor f) => (f x, k) -> f (x,k)
lstrength = undefined
  
-- | Right strength of a functor
rstrength :: (Functor f) => (k, f x) -> f (k,x)
rstrength = undefined

-- | Left Product of Automata
lprod :: (Ord a, Functor f, Functor g) => LTS f a s1 -> LTS g a s2 -> LTS (f :.: g) a (s1,s2)
lprod delta1 delta2 = fromFunction (nub (domain delta1 ++ domain delta2)) lprod'
     where lprod' a = Comp . fmap rstrength . lstrength . (delta1 \$ a *** delta2 \$ a)


-- | Right Product of Automata
rprod :: (Ord a, Functor f, Functor g) => LTS f a s1 -> LTS g a s2 -> LTS (g :.: f) a (s1,s2)
rprod delta1 delta2 = fromFunction (nub (domain delta1 ++ domain delta2)) rprod'
     where rprod' a = Comp . fmap lstrength . rstrength . (delta1 \$ a *** delta2 \$ a)


-- | Left Product of a Monadic Automata 
lprodM :: (Ord a, Functor f, Monad f) => LTS f a s1 -> LTS f a s2 -> LTS f a (s1,s2)
lprodM delta1 delta2 =  (joinC.)  \.  lprod delta1 delta2

rprodM :: (Ord a, Functor f, Monad f) => LTS f a s1 -> LTS f a s2 -> LTS f a (s1,s2)
rprodM delta1 delta2 =  (joinC.)  \.  rprod delta1 delta2

joinC :: (Monad f) => (f :.: f) x -> f x
joinC = join . unComp





----------------------------------------------
-- Bisimilarity
----------------------------------------------

-- | checks whether two states are bisimilar
bisimilar :: (Eq s, Ord a, GEq f) => LTS f a s -> s -> s -> Bool
bisimilar delta p q = runReader (bisim delta p q) [ ]

bisim :: (Eq s, Ord a, GEq f) => LTS f a s -> s -> s -> Reader [(s,s)] Bool
bisim delta p q = do  stack <- ask
                      if p==q || (p,q) `elem` stack || (q,p) `elem` stack
                         then return True
                         else liftM and $ mapM (bisimBy delta p q) (domain delta)


bisimBy :: (Eq s, Ord a, GEq f) => LTS f a s -> s -> s -> a -> Reader [(s,s)] Bool
bisimBy delta p q a =  let  p' = (delta \$ a) p 
                            q' = (delta \$ a) q 
                       in   local ((p,q):) $
                              eqByM (bisim delta) p' q'


