{-#LANGUAGE KindSignatures#-}
{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE FlexibleContexts#-}

module ParRegular.Representations 
(

  -- * BiFunctorial structural representation types.
  Rec (..),
  Par (..),
  K (..),
  Unit (..),
  (:+:) (..),
  (:*:) (..),
  Con (..),
  PSet (..),
  (:^:) (..),
  
  -- * Fixed-point type.
  Fix (..),
  BFix (..),

  -- * Type class capturing the structural representation of a parametrized type and the
  -- | corresponding embedding-projection pairs.
  ParRegular (..),
  EParRegular (..),

  -- | Structural representation of composition of functors
  (:.:) (..)
  
) 
where

import ParRegular.BiFunctor
import ParRegular.EqFunctor

import ParRegular.Data.ESet
import ParRegular.Data.FiniteFunctions

-----------------------------------------------------------------------------
-- BiFunctorial structural representation types.
-----------------------------------------------------------------------------

-- | Structure type for constant values.
data K k a r      = K k

-- | Structure type for recursive values.
data Rec a r       = Rec r

-- | Structure type for occurrences of the type parameter
data Par a r = Par a

-- | Structure type for empty constructors.
data Unit a r     = Unit

-- | Structure type for alternatives in a type.
data (f :+: g) a r  = L (f a r) | R (g a r)

-- | Structure type for fields of a constructor.
data (f :*: g) a r = f a r :*: g a r

-- | Structure type to store the name of a constructor.
data Con f a r    = Con String (f a r)

-- | Structure for powerset
data PSet f a r = PSet (ESet (f a r))

-- | Structure for exponentials 
data (f :^: k) a r = Exp (k :-> f a r)


infixr 6 :+:
infixr 7 :*:
infix  8 :^:

-----------------------------------------------------------------------------
-- Fixed-point type.
-----------------------------------------------------------------------------

-- | The well-known fixed-point type.
newtype Fix f = In (f (Fix f))

newtype BFix f a = InB {unInB :: f a (BFix f a)}

--instance BiFunctor f => Functor (BFix f) where
--   fmap f = InB . bimap f (fmap f) . unInB

-----------------------------------------------------------------------------
-- Type class capturing the structural representation of a type and the
-- | corresponding embedding-projection pairs.
-----------------------------------------------------------------------------

-- | The type class @Regular@ captures the structural representation of a 
-- type and the corresponding embedding-projection pairs.


class BiFunctor (PBF t) => ParRegular t where
  type PBF t  :: * -> * -> *
  from        :: t a -> PBF t a (t a)
  to          :: PBF t a (t a) -> t a

class EBiFunctor (EPBF t) => EParRegular t where
  type EPBF t  :: * -> * -> *
  efrom        :: t a -> EPBF t a (t a)
  eto          :: EPBF t a (t a) -> t a


-- | Structure type for the composition of two functors  
data (f :.: g) x = Comp {unComp :: f (g x)}



























