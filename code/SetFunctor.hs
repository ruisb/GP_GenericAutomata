{-# LANGUAGE UndecidableInstances, FlexibleContexts, KindSignatures, FlexibleInstances #-}
module SetFunctor where

import Data.List (nub)

data Set a = S {unS :: [a]}

setmap :: (Eq a , Eq b) => (a -> b) -> Set a -> Set b
setmap f = S . nub . map f . unS


newtype PSet f r = PSet (Set (f r))


--THIS DOES NOT WORK
--instance (Functor f, Eq1 f) => Functor (PSet f) where
--  fmap f (PSet xs) = PSet (setmap (fmap f) xs)
--
class Eq1 (f :: * -> *) where
   eq1 :: (Eq a) => f a -> f a -> Bool

instance (Eq1 f, Eq a) => Eq (f a) where
  (==) = eq1



class EFunctor f where
 efmap :: (Eq a, Eq b) => (a -> b) -> f a -> f b

instance (Functor f) => EFunctor f where
  efmap = fmap

instance (Functor f, Eq1 f) => EFunctor (PSet f) where
  efmap f (PSet xs) = PSet (setmap (fmap f) xs)

