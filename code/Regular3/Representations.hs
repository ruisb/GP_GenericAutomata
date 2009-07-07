{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances   #-}
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

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

-----------------------------------------------------------------------------
-- BiFunctorial structural representation types.
-----------------------------------------------------------------------------

-- | Structure type for constant values.
data K k a r      = K k

-- | Structure type for recursive values.
data Rec a r       = Rec r

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

data Par a r = Par a

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

class BiFunctor (PBF t) => ParRegular t where
  type PBF t :: * -> * -> *
  from :: t a -> PBF t a (t a)
  to   :: PBF t a (t a) -> t a

  
class BiFunctor (f :: * -> * -> *) where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance BiFunctor f => Functor (f a) where
  fmap = bimap id 

newtype BFix f a = InB {unInB :: f a (BFix f a)}

instance BiFunctor f => Functor (BFix f) where
   fmap f = InB . bimap f (fmap f) . unInB

type (:->) = Map 

infixl 9 \$
(\$) :: (Ord a) => (a :-> b) -> a -> b
f \$ x = guardFromJust "finite function not defined" (M.lookup x f)
guardFromJust _   (Just x) = x
guardFromJust err Nothing  = error err

infixr 9 \.
(\.) :: (Ord a) => (b -> c) -> (a :-> b) -> (a :-> c)
g \. f = M.map g f


domain :: (Ord a) => (a :-> b) -> [a]
domain = M.keys

range   :: (Ord a) => (a :-> b) -> [b] 
range   = M.elems



instance (ParRegular f) => Functor f where
   fmap f  =  to . bimap f (fmap f)  . from

data (f :.: g) x = Comp {unComp :: f (g x)}
instance (Functor f, Functor g) => Functor (f :.: g) where
   fmap f = Comp . fmap (fmap f) . unComp
instance (EFunctor f, EFunctor g, Ord1 g, Ord1 f) => EFunctor (f :.: g) where
   efmap f = Comp . efmap (efmap f) . unComp


--this is to be put in the other module: Base!

data Crusher a r = Crusher { plus  :: r -> r -> r
            		   , zero  :: r
                           , inj   :: a -> r
            		   }

class BCrush f where
  bcrush :: Crusher a r -> f a r -> r

instance BCrush Rec where
  bcrush _ (Rec x) = x

instance BCrush Par where
  bcrush m (Par x) = (inj m) x
--
instance BCrush (K k) where
  bcrush m _ = zero m

instance BCrush Unit where
  bcrush m _ = zero m

instance (BCrush f, BCrush g) => BCrush (f :+: g) where
  bcrush m (L x) = bcrush m x
  bcrush m (R y) = bcrush m y

instance (BCrush f, BCrush g) => BCrush (f :*: g) where
  bcrush m (x :*: y) = (plus m) (bcrush m x) (bcrush m y)  
  -- = bcrushr op (bcrush op e y) x

instance BCrush f => BCrush (Con f) where
  bcrush m (Con _ x) = bcrush m x

--instance BCrush f => Crush (PSet f) where
--  bcrush op e (PSet s) = crush' op e $ S.toList s
--
---- Not sure if it's correct
--instance BCrush f => Crush (f :^: a) where
--  bcrush op e (Exp mp) = crush' op e $ map snd $ M.toList mp
--
---- helper
--bcrush' op e xs = case xs of
--  []   -> e
--  x:xs -> bcrush' op (crush op e x) xs
--
--
 
class Crush f where
  crush ::  Crusher a b -> f a -> b

instance (ParRegular t, BCrush (PBF t)) => Crush t where
  crush m = fold (bcrush m)


instance (Crush f, Crush g) => Crush (f :.: g) where
 crush m  = crush m{inj = crush m}  . unComp


data L a = Nil | Cons a (L a)
  deriving (Show)--,Eq,Ord)

instance ParRegular L where
  type PBF L = Unit :+: Par :*: Rec
  to (L Unit) = Nil
  to (R (Par x :*: Rec xs)) = Cons x xs
  from Nil  = L Unit
  from (Cons x xs) = R (Par x :*: Rec xs)

data T a = Leaf a | Fork (T a) (T a)

instance ParRegular T where
   type PBF T = Par :+: Rec :*: Rec
   to (L (Par x)) = Leaf x
   to (R (Rec l :*: Rec r)) = Fork l r
   from (Leaf x) = L (Par x)
   from (Fork l r) = R (Rec l :*: Rec r)






toSetCrusher :: (Ord a) => Crusher a (Set a)
toSetCrusher = Crusher S.union S.empty S.singleton

toSet :: (Ord a, Crush f) => f a -> Set a 
toSet = crush toSetCrusher

instance BiFunctor Unit where
  bimap _ _   Unit        = Unit

instance BiFunctor (K k) where
  bimap _ _  (K k)       = K k

instance (BiFunctor f, BiFunctor g) => BiFunctor (f :+: g) where
  bimap f g  (L x)       = L (bimap f g x)
  bimap f g  (R y)       = R (bimap f g y)

instance (BiFunctor f, BiFunctor g) => BiFunctor (f :*: g) where
  bimap f g  (x :*: y)   = bimap f g x :*: bimap f g y

instance BiFunctor Rec where
  bimap f g  (Rec r)     = Rec (g r)

instance BiFunctor Par where
  bimap f g  (Par a)     = Par (f a)

instance (Ord k, BCrush f) => BCrush (f :^: k) where
   bcrush m (Exp g) = foldr (\x r -> (plus m) (bcrush m x) r) (zero m) (range g)


instance BCrush f => BCrush (PSet f) where
   bcrush m (PSet x) = S.fold (\x r -> (plus m) (bcrush m x) r) (zero m) x


type Algebra f x = f x -> x
fold :: (ParRegular t) => Algebra (PBF t a) r -> t a -> r
fold alg = alg . bimap id (fold alg). from
efold :: (EParRegular t, Ord a, Ord r, Ord (t a)) => Algebra (EPBF t a) r -> t a -> r
efold alg = alg . ebimap id (efold alg). efrom



instance ParRegular Maybe where
  type  PBF Maybe    = Unit :+: Par
  from  Nothing      = L Unit
  from  (Just x)     = R (Par x)
  to    (L Unit)     = Nothing
  to    (R (Par x))  = Just x



type CDA a s = LTS Id a s 
type PDA a s = LTS Maybe a s 
type NDA a s = LTS Set a s
type LSA a  s = LTS [] a s
newtype Id x = Id {unId :: x}
pda1 :: PDA Char Int
pda1 = M.fromList  [ ('a', funcFromList  [ (1,  Just 2   )
                                         , (2,  Nothing  )
                                         , (3,  Nothing  )
                                         ])
                   , ('b', funcFromList  [ (1,  Just 3   )
                                         , (2,  Just 3   )
                                         , (3,  Nothing  )
					 ])
                   ]

lsa1 :: LSA Char Int
lsa1 = M.fromList  [ ('a', funcFromList  [ (1,  [2,1]    )
                                         , (2,  []       )
                                         , (3,  []       )
                                         ])
                   , ('b', funcFromList  [ (1,  [3]      )
                                         , (2,  [1,3]    )
                                         , (3,  []       )
					 ])
                   ]
		  
funcFromList :: (Eq a) => [(a,b)] -> a -> b
funcFromList assoc x = guardFromJust "function not defined" (lookup x assoc)

-- Transition system
type TS f s = s -> f s

-- Labeled transition system
type LTS f a s = Map a (TS f s)

data Star f x = End x | Step (f (Star f x)) 

instance (Show1 f, Show x) => Show (Star f x) where
  show (End x) = "End " ++ show x
  show (Step fx) = "Step " ++ show1 fx 

class Show1 f where
  show1 :: (Show x) => f x -> String

instance (Show x, Show1 f) => Show (f x) where
  show = show1

instance Show1 [] where
 show1 = show

runPatt :: (Ord a,Functor f) => (s -> res, f res -> res) -> LTS f a s -> [a] -> s -> res
runPatt (eta,mu) delta []      = eta
runPatt (eta,mu) delta (a:as)  = mu . fmap (runPatt (eta,mu) delta as) . (delta \$ a)


runForgetful :: (Ord a, Ord s, Crush f, Functor f) =>  LTS f a s -> [a] -> s -> Set s
runForgetful = runPatt (S.singleton, setjoin . toSet)
  where  toSet :: (Ord a, Crush f) => f a -> Set a
         toSet = crush Crusher{plus=S.union, zero=S.empty, inj=S.singleton}
         setjoin :: (Ord a) => Set (Set a) -> Set a
	 setjoin = S.fold S.union S.empty


runPreserving :: (Ord a, Functor f) => LTS f a s -> [a] -> s -> Star f s
runPreserving = runPatt (End,Step)

runInMonad :: (Ord a, Monad f, Functor f) =>  LTS f a s -> [a] -> s -> f s
runInMonad = runPatt (return,join)


erunPatt :: (Ord a,EFunctor f, Ord s, Ord res) => (s -> res, f res -> res) -> LTS f a s -> [a] -> s -> res
erunPatt (eta,mu) delta []      = eta
erunPatt (eta,mu) delta (a:as)  = mu . efmap (erunPatt (eta,mu) delta as) . (delta \$ a)


erunForgetful :: (Ord a, Ord s, Crush f, EFunctor f) =>  LTS f a s -> [a] -> s -> Set s
erunForgetful = erunPatt (S.singleton, setjoin . toSet)
  where  toSet :: (Ord a, Crush f) => f a -> Set a
         toSet = crush Crusher{plus=S.union, zero=S.empty, inj=S.singleton}
         setjoin :: (Ord a) => Set (Set a) -> Set a
	 setjoin = S.fold S.union S.empty


erunPreserving :: (Ord a, EFunctor f, Ord s, Ord (Star f s)) => LTS f a s -> [a] -> s -> Star f s
erunPreserving = erunPatt (End,Step)

erunInMonad :: (Ord a, Monad f, EFunctor f, Ord s, Ord (f s)) =>  LTS f a s -> [a] -> s -> f s
erunInMonad = erunPatt (return,join)



instance ParRegular [] where
  type PBF []= Unit :+: Par :*: Rec
  from [] = L Unit
  from (x:xs) = R (Par x :*: Rec xs)
  to (L Unit) = []
  to (R (Par x:*:Rec xs)) = x : xs


data WeightedTrans s = WT { weight :: Int, nextst :: s} deriving (Show) --, Eq, Ord)
type NDWeightTrans = Set :.: WeightedTrans

type WeightedAut s = LTS NDWeightTrans () s

instance ParRegular WeightedTrans where
  type PBF WeightedTrans =  (K Int :*: Par)
--  efrom = PSet . S.map (\(WT w s) -> K w :*: Par s) . unNWT
--  eto   = NWT . S.map (\(K w :*: Par s) -> WT w s) - unPSet
  from (WT w s) = K w :*: Par s
  to (K w :*: Par s) = WT w s

instance EParRegular Set where
  type EPBF Set = PSet Par
  efrom = PSet . S.map Par 
  eto   = S.map unPar . unPSet

unPSet (PSet s) = s
unPar (Par x ) = x

instance Ord1 WeightedTrans where
  compare1 (WT w1 s1) (WT w2 s2)
           | s1 < s2   = LT
           | s1 > s2   = GT                    
           | w1 < w2   = LT
           | w1 > w2   = GT                    
           | otherwise = EQ

wa1 :: WeightedAut Int
wa1 = M.fromList [( () , 
       funcFromList [ (1 , Comp $ S.fromList [WT 1 3, WT 2 2 , WT 5 3] ) 
                    , (2 , Comp $ S.fromList [WT 4 2, WT 7 4] )
                    , (3 , Comp $ S.fromList [WT 1 3, WT 2 2, WT 5 1] )
                    , (4 , Comp $ S.fromList [WT 1 2, WT 0 4] )
                    ]
      )]

pulse n = replicate n ()

instance Monad WeightedTrans where
   return x = WT 0 x
   (WT w1 x) >>= k = let WT w2 y = k x
                     in WT (w1+w2) y

class EBiFunctor (EPBF t) => EParRegular t where
  type EPBF t  :: * -> * -> *
  efrom        :: (Ord a) => t a -> (EPBF t) a (t a)
  eto          :: (Ord a) => (EPBF t) a (t a) -> t a

class EFunctor f where
 efmap  :: (Ord a, Ord b) => (a -> b) -> f a  -> f b 
--instance (Functor f) => EFunctor f where
--  efmap = fmap

class EBiFunctor f where
 ebimap  :: (Ord a, Ord b, Ord c, Ord d) => (a -> b) -> (c ->d) -> f a c -> f b d
instance (BiFunctor f) => EBiFunctor f where
  ebimap  = bimap

instance (BiFunctor f, Ord2 f) => EBiFunctor (PSet f) where
  ebimap f g (PSet xs) = PSet (S.map (bimap f g) xs)

instance Ord2 Par where
  compare2 = compare  
  
class  Ord2 (f :: * -> * -> *) where
   compare2 :: (Ord a, Ord b) => f a b -> f a b -> Ordering 
--instance (Ord2 f, Ord a, Ord b) => Eq (f a b) where
--  x == y = compare2 x y == EQ
--instance (Ord2 f, Ord a, Ord b) => Ord (f a b) where
--  compare = compare2
instance (Ord2 f, Ord a) => Ord1 (f a) where
  compare1 = compare2

class  Ord1 (f :: * -> *) where
   compare1 :: (Ord a) => f a -> f a -> Ordering 
instance (Ord a , Ord1 f) => Eq (f a ) where
  x == y =  compare x y == EQ
instance (Ord a , Ord1 f) => Ord (f a ) where
  compare = compare1

