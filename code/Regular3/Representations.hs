{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Rewriting.Representations
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Types for structural representation.
-----------------------------------------------------------------------------

module Regular3.Representations 
--(
--
--  -- * Functorial structural representation types.
--  K (..),
--  Id (..),
--  Unit (..),
--  (:+:) (..),
--  (:*:) (..),
--  Con (..),
--  PSet (..),
--  (:^:) (..),
--  
--  -- * Fixed-point type.
--  Fix (..),
--
--  -- * Type class capturing the structural representation of a type and the
--  -- | corresponding embedding-projection pairs.
--  Regular (..)
--  
--) 
where

import Data.Set
import Data.Map

-----------------------------------------------------------------------------
-- BiFunctorial structural representation types.
-----------------------------------------------------------------------------

-- | Structure type for constant values.
data K k a r      = K k

-- | Structure type for recursive values.
data Id a r       = Id r

-- | Structure type for empty constructors.
data Unit a r     = Unit

-- | Structure type for alternatives in a type.
data (f :+: g) a r  = L (f a r) | R (g a r)

-- | Structure type for fields of a constructor.
data (f :*: g) a r = f a r :*: g a r

-- | Structure type to store the name of a constructor.
data Con f a r    = Con String (f a r)

-- | Structure for powerset
data PSet f a r = PSet (Set (f a r))

-- | Structure for exponentials 
data (f :^: k) a r = Exp (Map k (f a r))

data Param a r = Param a

infixr 6 :+:
infixr 7 :*:
infix  8 :^:

-----------------------------------------------------------------------------
-- Fixed-point type.
-----------------------------------------------------------------------------

-- | The well-known fixed-point type.
newtype Fix f = In (f (Fix f))


-----------------------------------------------------------------------------
-- Type class capturing the structural representation of a type and the
-- | corresponding embedding-projection pairs.
-----------------------------------------------------------------------------

-- | The type class @Regular@ captures the structural representation of a 
-- type and the corresponding embedding-projection pairs.
--
-- To be able to use the rewriting functions, the user is required to provide
-- an instance of this type class.
---class Functor (PF a) => Regular a where
--  type PF a :: * -> *
--  from      :: a -> PF a a
--  to        :: PF a a -> a

class BiFunctor (PBF t) => BiRegular t where
  type PBF t :: * -> * -> *
  from :: t a -> PBF t a (t a)
  to   :: PBF t a (t a) -> t a

  
class BiFunctor (f :: * -> * -> *) where
  bifmap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance BiFunctor f => Functor (f a) where
  fmap = bifmap id 

newtype BFix f a = InB (f a (BFix f a))

instance BiFunctor f => Functor (BFix f) where
   fmap = undefined




