{-# LANGUAGE FlexibleContexts   #-}
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

module Regular2.Representations (

  -- * Functorial structural representation types.
  K (..),
  Id (..),
  Unit (..),
  (:+:) (..),
  (:*:) (..),
  Con (..),
  PSet (..),
  (:^:) (..),
  list2l,l2list,
  List,
  G,
  
  -- * Fixed-point type.
  Fix (..),

  -- * Type class capturing the structural representation of a type and the
  -- | corresponding embedding-projection pairs.
  Regular (..)
  
) where

import Data.Set
import Data.Map

-----------------------------------------------------------------------------
-- Functorial structural representation types.
-----------------------------------------------------------------------------

type G a = Unit :+: K a :*: Id
type List a = Fix (G a)

l2list :: [a] -> List a
l2list [] =In $ L Unit
l2list (x:xs) = In $ R (K x :*: Id (l2list xs))

list2l :: List a -> [a]
list2l (In (L Unit)) = []
list2l (In (R (K x :*: Id s))) = x : list2l s

-- | Structure type for constant values.
data K a r      = K a

-- | Structure type for recursive values.
data Id r       = Id r

-- | Structure type for empty constructors.
data Unit r     = Unit

-- | Structure type for alternatives in a type.
data (f :+: g) r  = L (f r) | R (g r)

-- | Structure type for fields of a constructor.
data (f :*: g) r = f r :*: g r

-- | Structure type to store the name of a constructor.
data Con f r    = Con String (f r)

-- | Structure for powerset
data PSet f r = PSet (Set (f r))

-- | Structure for exponentials 
data (f :^: a) r = Exp (Map a (f r))

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
class Functor (PF a) => Regular a where
  type PF a :: * -> *
  from      :: a -> PF a a
  to        :: PF a a -> a


