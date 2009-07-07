{-# LANGUAGE TypeOperators,TypeSynonymInstances,UndecidableInstances, FlexibleContexts, KindSignatures, FlexibleInstances #-}
module SetFunctor where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M

data Set a = S {unS :: [a]}

setmap :: (Eq a , Eq b) => (a -> b) -> Set a -> Set b
setmap f = S . nub . map f . unS
--
--
--newtype PSet f r = PSet (Set (f r))
--
--
----THIS DOES NOT WORK
----instance (Functor f, Eq1 f) => Functor (PSet f) where
----  fmap f (PSet xs) = PSet (setmap (fmap f) xs)
----
class Eq2 (f :: * -> * -> *) where
   eq2 :: (Eq a, Eq r) => f a r -> f a r -> Bool

instance (Eq2 f, Eq a, Eq r) => Eq (f a r) where
  (==) = eq2
--
--
--
--class EFunctor f where
-- efmap :: (Eq a, Eq b) => (a -> b) -> f a -> f b
--
--instance (Functor f) => EFunctor f where
--  efmap = fmap
--
--instance (Functor f, Eq1 f) => EFunctor (PSet f) where
--  efmap f (PSet xs) = PSet (setmap (fmap f) xs)

data BPSet f a r = BPSet (Set (f a r))

class EBiFunctor f where
 ebimap :: (Eq a, Eq b, Eq c, Eq d) => (a -> b) -> (c ->d) -> f a c -> f b d
instance (BiFunctor f) => EBiFunctor f where
  ebimap = bimap
instance (BiFunctor f, Eq2 f) => EBiFunctor (BPSet f) where
  ebimap f g (BPSet xs) = BPSet (setmap (bimap f g) xs)
class BiFunctor (f :: * -> * -> *) where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d


data (f :.: g) x = Comp {unComp :: f (g x)}
instance (Functor f, Functor g) => Functor (f :.: g) where
   fmap f = Comp . fmap (fmap f) . unComp



instance (Ord k, BiFunctor f) => BiFunctor (f :^: k) where
  bimap f g = Exp . (bimap f g \.) . unExp

data (f :^: k) a r = Exp {unExp :: k :-> f a r}

type (:->) = Map 

infixl 9 \$
infixr 9 \.

(\$) :: (Ord a) => (a :-> b) -> a -> b
f \$ x = guardFromJust "finite function not defined" (M.lookup x f)
   where  guardFromJust _   (Just x) = x
          guardFromJust err Nothing  = error err

(\.) :: (Ord a) => (b -> c) -> (a :-> b) -> (a :-> c)
g \. f = M.map g f


