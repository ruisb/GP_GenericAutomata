{-#LANGUAGE OverlappingInstances#-}
-- | A not very efficient implementation of Sets, with only the Eq instance requirement
module ParRegular.Data.ESet where

import ParRegular.EqFunctor
import Data.List (nub)
import Control.Monad
import Control.Arrow ((***))

newtype ESet a = ESet {unESet :: [a]}
  deriving Show

-- | construction
empty = ESet []
singleton x = ESet [x]
insert x =  ESet . (x:) . filter (/=x) . unESet

union x y = fromList (unESet x ++ unESet y)

-- | query
isnull = null . unESet
size = length . unESet
member x = elem x . unESet

-- | from/to list
fromList :: (Eq a) => [a] -> ESet a
fromList = ESet . nub
toList :: ESet a -> [a]
toList   = unESet

-- | functor (map)

instance EFunctor ESet where
  emap  = setmap


setmap :: (Eq a, Eq b) => (a -> b) -> ESet a -> ESet b
setmap f = fromList . map f . toList 

setmapM :: (Monad m, Eq a, Eq b) => (a -> m b) -> ESet a -> m (ESet b)
setmapM f = liftM fromList . mapM f . toList

-- | fold
setfold f z = foldr f z . unESet


-- | unions
setjoin :: (Eq a) => ESet (ESet a) -> ESet a
setjoin = setfold union empty

-- | monad
instance EMonad ESet where
  ereturn  = singleton
  ejoin    = setjoin


-- | comparison
instance Eq1 ESet where
   eq1 = seteq 

seteq x y = x `subset` y && y `subset` x

subset x y = all (\a -> a `elem` unESet y) (unESet x)


seteqBy :: (a -> a -> Bool) -> ESet a -> ESet a -> Bool
seteqBy op x y = subsetBy op x y && subsetBy op y x

subsetBy :: (a -> a -> Bool) -> ESet a -> ESet a -> Bool
subsetBy op x y = all (\a -> any (\b -> a `op` b) (unESet y)) (unESet x)

seteqByM :: (Monad m) => (a -> a -> m Bool) -> ESet a -> ESet a -> m Bool
seteqByM op x y = liftM2 (&&) (subsetByM op x y) (subsetByM op y x)

subsetByM :: (Monad m) => (a -> a -> m Bool) -> ESet a -> ESet a -> m Bool
subsetByM op x y = allM (\a -> anyM (\b -> a `op` b) (unESet y)) (unESet x)

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM f = liftM and . mapM f

anyM ::(Monad m) =>  (a -> m Bool) -> [a] -> m Bool
anyM f = liftM or . mapM f

