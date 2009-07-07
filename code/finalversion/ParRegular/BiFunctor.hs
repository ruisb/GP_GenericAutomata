{-#LANGUAGE FlexibleInstances#-}
module ParRegular.BiFunctor where

class BiFunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance BiFunctor f => Functor (f a) where
  fmap = bimap id 

