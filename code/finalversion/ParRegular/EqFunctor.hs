{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE OverlappingInstances#-}
module ParRegular.EqFunctor where

class EFunctor f where
  emap :: (Eq a, Eq b) => (a -> b) -> f a -> f b

class EBiFunctor f where
  ebimap :: (Eq a, Eq b, Eq c, Eq d) => (a -> b) -> (c -> d) -> f a c -> f b d

class  Eq1 f  where
   eq1 :: (Eq a) => f a -> f a -> Bool 
instance (Eq a , Eq1 f) => Eq (f a) where
  (==) = eq1

class  Eq2 f where
   eq2 :: (Eq a, Eq b) => f a b -> f a b -> Bool 
instance (Eq2 f, Eq a) => Eq1 (f a) where
  eq1 = eq2



class (EFunctor m) => EMonad m where
   ereturn  :: (Eq a) => a -> m a
   ejoin    :: (Eq a) => m (m a) -> m a

ebind :: (Eq a, Eq b, Eq1 m, EMonad m) => m a -> (a -> m b) -> m b
ebind x g = ejoin (emap g x) 

ekleisli :: (Eq a, Eq b, Eq c, Eq1 m, EMonad m) => (b -> m c) -> (a -> m b) -> a -> m c
ekleisli f g = ejoin . emap f . g

