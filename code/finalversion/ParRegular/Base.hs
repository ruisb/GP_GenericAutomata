{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE UndecidableInstances#-}
{-#LANGUAGE OverlappingInstances#-}
module ParRegular.Base  where

import ParRegular.Representations

import ParRegular.BiFunctor
import ParRegular.EqFunctor

import ParRegular.Data.FiniteFunctions
import ParRegular.Data.ESet 

import Control.Monad
import Control.Arrow
import Data.Function(on)

-----------------------------------------------------------------------------
-- Catamorphisms for parametrized regular data types
-----------------------------------------------------------------------------

type Algebra f x = f x -> x

fold :: (ParRegular t) => Algebra (PBF t a) r -> t a -> r
fold alg = alg . bimap id (fold alg). from

efold :: (Eq1 t, Eq a, Eq r, EParRegular t) => Algebra (EPBF t a) r -> t a -> r
efold alg = alg . ebimap id (efold alg). efrom


-----------------------------------------------------------------------------
-- Anamorphisms for parametrized regular data types
-----------------------------------------------------------------------------

type CoAlgebra f x = x -> f x 

unfold :: (ParRegular t) => CoAlgebra (PBF t a) r -> r -> t a
unfold coalg = to . bimap id (unfold coalg). coalg

eunfold :: (Eq1 t, Eq a, Eq r, EParRegular t) => CoAlgebra (EPBF t a) r -> r -> t a
eunfold coalg = eto . ebimap id (eunfold coalg). coalg


-----------------------------------------------------------------------------
-- Functorial map function.
-----------------------------------------------------------------------------


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

instance (Ord k, BiFunctor f) => BiFunctor (f :^: k) where
  bimap f g (Exp m)      = Exp (bimap f g \. m)

instance (Eq2 f, BiFunctor f) => EBiFunctor (PSet f) where
  ebimap f g (PSet xs)   = PSet (setmap (bimap f g) xs)


  
instance (ParRegular f) => Functor f where
    fmap f  =  to . bimap f (fmap f)  . from
  
instance (Eq1 f, EParRegular f) => EFunctor f where
    emap f  =  eto . ebimap f (emap f)  . efrom
  
instance (Functor f, Functor g) => Functor (f :.: g) where
   fmap f = Comp . fmap (fmap f) . unComp

instance (Eq1 f, Eq1 g, EFunctor f, EFunctor g) => EFunctor (f :.: g) where
   emap f = Comp . emap (emap f) . unComp


-----------------------------------------------------------------------------
-- Monadic functorial map function.
-----------------------------------------------------------------------------

class BiFunctorM f where
   bimapM :: (Monad m) => (a -> m b) -> (c -> m d) -> f a c -> m (f b d)  
class EBiFunctorM f where
   ebimapM ::  (Eq a, Eq b, Eq c, Eq d, Monad m) => (a -> m b) -> (c -> m d) -> f a c -> m (f b d)  

instance BiFunctorM Unit where
  bimapM _ _   Unit        = return Unit

instance BiFunctorM (K k) where
  bimapM _ _  (K k)       = return (K k)

instance (BiFunctorM f, BiFunctorM g) => BiFunctorM (f :+: g) where
  bimapM f g  (L x)       = liftM L (bimapM f g x)
  bimapM f g  (R y)       = liftM R (bimapM f g y)

instance (BiFunctorM f, BiFunctorM g) => BiFunctorM (f :*: g) where
  bimapM f g  (x :*: y)   = liftM2 (:*:) (bimapM f g x) (bimapM f g y)

instance BiFunctorM Rec where
  bimapM f g  (Rec r)     = liftM Rec (g r)

instance BiFunctorM Par where
  bimapM f g  (Par a)     = liftM Par (f a)

instance (Ord k, BiFunctorM f) => BiFunctorM (f :^: k) where
  bimapM f g (Exp m)      = liftM Exp $ ff_sequence (bimapM f g \. m)

instance (Eq2 f, BiFunctorM f) => EBiFunctorM (PSet f) where
  ebimapM f g (PSet xs)   = liftM PSet (setmapM (bimapM f g) xs)


class FunctorM f where
   fmapM :: (Monad m) => (a -> m b) -> f a -> m (f b)  
class EFunctorM f where
   emapM ::  (Eq a, Eq b, Monad m) => (a -> m b) -> f a  -> m (f b)  



instance (ParRegular f, BiFunctorM (PBF f)) => FunctorM f where
    fmapM f  =  liftM to . bimapM f (fmapM f)  . from
  
instance (Eq1 f, EParRegular f, EBiFunctorM (EPBF f)) => EFunctorM f where
    emapM f  =  liftM eto . ebimapM f (emapM f)  . efrom

instance (FunctorM f, FunctorM g) => FunctorM (f :.: g) where
   fmapM f = liftM Comp  . fmapM (fmapM f) . unComp

instance (Eq1 f, Eq1 g, EFunctorM f, EFunctorM g) => EFunctorM (f :.: g) where
   emapM f = liftM Comp . emapM (emapM f) . unComp
--

-----------------------------------------------------------------------------
-- Crush functions.
-----------------------------------------------------------------------------


--this is to be put in the other module: Base!

data Crusher a r = Crusher { plus  :: r -> r -> r
            		   , zero  :: r
                           , inj   :: a -> r
            		   }

class BCrush f where
  bcrush :: Crusher a r -> Algebra (f a) r

instance BCrush Rec where
  bcrush _ (Rec x) = x

instance BCrush Par where
  bcrush m (Par x) = (inj m) x

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


instance (Ord k, BCrush f) => BCrush (f :^: k) where
   bcrush m (Exp g) = foldr (\x r -> (plus m) (bcrush m x) r) (zero m) (range g)


instance BCrush f => BCrush (PSet f) where
   bcrush m (PSet x) = setfold (\x r -> (plus m) (bcrush m x) r) (zero m) x


 
class Crush f where
  crush ::  Crusher a b -> f a -> b

instance (ParRegular t, BCrush (PBF t)) => Crush t where
  crush m = fold (bcrush m)

instance (Crush f, Crush g) => Crush (f :.: g) where
 crush m  = crush m{inj = crush m}  . unComp

 
-- | Flatten a structure by collecting all the elements present.
flatten :: Crush f => f a -> [a]
flatten = crush Crusher{plus = (++) , zero = [], inj = (:[]) }
  

-----------------------------------------------------------------------------
-- Zip functions.
-----------------------------------------------------------------------------

-- maybe the type is a bit awkward: the most generic case we consider is a maybe result (that encodes structural fail) togheter with a monad.. this can be done better probably: it is too specific for our application to bisimilarity.

class BZip f where
  bfSafeZipM :: (Monad m) => (a -> b -> m c) -> (ra -> rb -> m (Maybe r)) -> f a ra -> f b rb -> m (Maybe (f c r))

liftMM :: (Monad m1,Monad m2) => (a -> b) -> m1 (m2 a) -> m1 (m2 b)
liftMM = liftM.liftM

liftMM2 :: (Monad m1,Monad m2) => (a -> b -> c) -> m1 (m2 a) -> m1 (m2 b) -> m1 (m2 c)
liftMM2 = liftM2 . liftM2 

returnMM :: (Monad m1,Monad m2) => a -> m1 (m2 a)
returnMM = return . return

sequenceMM :: (Monad m1, Monad m2) => [m1 (m2 a)] -> m1 (m2 [a])
sequenceMM = liftM sequence . sequence

instance BZip Rec where
  bfSafeZipM op rc (Rec x) (Rec y) = (liftMM Rec) (rc x y)

instance BZip Par where
  bfSafeZipM op rc (Par x) (Par y) =  liftM (Just . Par) (x `op` y)

instance (Eq k) => BZip (K k) where
  bfSafeZipM op rc (K a) (K b) 
      | a == b     = return $ Just (K a) 
      | otherwise  = return Nothing

instance BZip Unit where
  bfSafeZipM op rc Unit Unit = return $ Just Unit

instance (BZip f, BZip g) => BZip (f :+: g) where
  bfSafeZipM op rc (L x) (L y)  = liftMM L (bfSafeZipM op rc x y)
  bfSafeZipM op rc (R x) (R y)  = liftMM R (bfSafeZipM op rc x y)
  bfSafeZipM op rc _     _      = return Nothing

instance (BZip f, BZip g) => BZip (f :*: g) where
  bfSafeZipM op rc (x1 :*: x2) (y1 :*: y2) = liftMM2 (:*:) (bfSafeZipM op rc x1 y1) (bfSafeZipM op rc x2 y2)  

instance BZip f => BZip (Con f) where
  bfSafeZipM op rc (Con nm x) (Con nm' y) 
      | nm == nm' = liftMM (Con nm) (bfSafeZipM op rc x y)
      | otherwise = return Nothing

instance (Ord k, BZip f) => BZip (f :^: k) where
   bfSafeZipM op rc (Exp g1) (Exp g2) 
      | domain g1 == domain g2 = let zipA a = bfSafeZipM op rc (g1 \$ a) (g2 \$ a)
                                 in  liftMM Exp . liftMM ff_fromList . sequenceMM . map (uncurry (liftMM2 (,))) . map (returnMM &&& zipA) $ domain g1
      | otherwise              = return Nothing



class Zip f where
  fSafeZipM :: (Monad m) => (a -> b -> m c) -> f a -> f b -> m (Maybe (f c))

instance (ParRegular t, BZip (PBF t)) => Zip t where
  fSafeZipM op t1 t2 = liftMM to $ bfSafeZipM op (fSafeZipM op) (from t1) (from t2)

instance (Zip f, Zip g, FunctorM f) => Zip (f :.: g) where
  fSafeZipM op x y = let zipComp op = fSafeZipM (fSafeZipM op) 
                         flat       = liftM Comp . join . liftM (fmapM id)
                     in  liftM flat (zipComp op (unComp x) (unComp y))
          

bfSafeZip ::  (BZip f) => (a -> b -> c) -> (ra -> rb -> Maybe r) -> f a ra -> f b rb -> Maybe (f c r)
bfSafeZip op rc x y = unI $ bfSafeZipM (\x y -> I (x `op` y)) (\x y -> I (rc x y)) x y

fSafeZip ::  (Zip f) => (a -> b -> c) -> f a -> f b -> Maybe (f c)
fSafeZip op x y = unI $ fSafeZipM (\x y -> I (x `op` y)) x y

newtype I x = I {unI::x}
instance Monad I where
  return = I
  (I x) >>= f = f x


-----------------------------------------------------------------------------
-- Equality function.
-----------------------------------------------------------------------------

class BEq f where
  beqByM :: (Monad m) => (a -> a -> m Bool) -> (ra -> ra -> m Bool) -> f a ra -> f a ra -> m Bool

instance (BZip f, BCrush f) => BEq f where
  beqByM op rc x y = liftM (maybe False fand) $ bfSafeZipM op (\x y-> liftM Just (rc x y)) x y


fand :: (BCrush f) => f Bool Bool -> Bool
fand = bcrush Crusher{ plus = (&&), zero = False, inj = id}

instance (BEq f) => BEq (PSet f) where
  beqByM op rc  (PSet x) (PSet y) = seteqByM (beqByM op rc) x y

class GEq f where
  eqByM :: (Monad m) => (a -> a -> m Bool) -> f a -> f a -> m Bool

instance (ParRegular t, BEq (PBF t)) => GEq t where
  eqByM op t1 t2 = beqByM op (eqByM op) (from t1) (from t2)

instance (GEq f, GEq g, FunctorM f) => GEq (f :.: g) where
  eqByM op x y = eqByM (eqByM op) (unComp x) (unComp y)





-----------------------------------------------------------------------------
-- Show function.
-----------------------------------------------------------------------------

-- TODO
-----------------------------------------------------------------------------


