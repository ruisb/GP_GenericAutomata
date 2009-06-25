{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-#LANGUAGE FlexibleInstances#-}
module Generic where

import Debug.Trace

import Regular2.Representations
import Regular2.Base

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Control.Arrow
import Control.Monad.Reader
import Data.Maybe




-- Transition system
type TS f s = s -> f s

-- Labeled transition system
type LTS a f s = Map a (TS f s)

data Star f s = End s
              | Step (f (Star f s))

class Show1 (f :: * -> *) where
 show1 :: (Show s) => f s -> String

instance (Show1 f, Show s) => Show (Star f s) where
  show (End x)    = {--"End" ++ --}show x
  show (Step x)= {--"Step" ++ --}show1 x

instance Show1 [] where
  show1 = show

--instance (Show1 f, Show s) => Show (f s) where
 -- show = show1

toSet :: (Crush f, Ord a) => f a -> Set a
toSet fa = crush S.insert S.empty fa

setjoin :: (Ord a) => Set (Set a) -> Set a
setjoin = S.fold S.union S.empty

run1 :: (Ord a, Ord s, Crush f, Functor f) =>
        LTS a f s -> [a] -> s -> Set s
run1 lts xs s = case xs of
  []   -> S.singleton s
  y:ys -> case M.lookup y lts of
    Nothing -> error "Incomplete automaton!" -- incomplete automaton, no transition
    Just ts -> let fs = fmap (run1 lts ys) $ ts s
               in  S.fold S.union S.empty $ toSet fs

runForgetful :: (Ord a, Ord s, Crush f, Functor f) =>
                LTS a f s -> [a] -> s -> Set s
runForgetful delta []      = S.singleton 
runForgetful delta (a:as)  = setjoin . toSet . fmap (runForgetful delta as) . (delta \$ a)


--runForgetful' :: (Ord a, Ord s, Functor f, Regular (f s)) => LTS a f s -> [a] -> s -> Set s
--runForgetful' delta []     s = S.singleton s
--runForgetful' delta (a:as) s = {--setjoin . toSet . fmap (runForgetful' delta as)--}undefined $ (from ( (delta \$ a) s) :: PF (f s) (f s) )

run2 :: (Ord a, Ord s, Functor f) =>
        LTS a f s -> [a] -> s -> Star f s
run2 lts xs s = case xs of
  []   -> End s
  y:ys -> case M.lookup y lts of
    Nothing -> error "Incomplete automaton!"
    Just ts -> Step $ fmap (run2 lts ys) $ ts s

runPreserving :: (Ord a, Ord s, Functor f) =>
        LTS a f s -> [a] -> s -> Star f s
runPreserving delta []      = End 
runPreserving delta (a:as)  = Step . fmap (runPreserving delta as) . (delta \$ a)

run3 :: (Ord a, Ord s, Monad f) => LTS a f s -> [a] -> s -> f s
run3 lts xs s = case xs of
  []   -> return s
  y:ys -> case M.lookup y lts of
    Nothing -> fail "Incomplete automaton!"
    Just ts -> ts s >>= run3 lts ys


runInMonad :: (Ord a, Ord s, Functor f, Monad f) => LTS a f s -> [a] -> s -> f s
runInMonad delta []      = return
runInMonad delta (a:as)  = runInMonad delta as <=< delta \$ a
--run3' delta (a:as)  = join  . fmap (run3' delta as) . (delta \$ a)

-- I now think product canot be defined so generically...
--prod :: (Functor f) => LTS a f s1 -> LTS a f s2 -> LTS a f (s1,s2)
--prod t1 t2 = let alph = M.keys t1
--             in  M.fromList . map (\a ->(a, t1 \$ a *** t2 \$ a)) $ alph
--

--these are utility functions  to deal with finite function. 
-- we do not want to always care about them being a map.
-- todo: think how they could be done well
infixl 9 \$
m \$ k = guardFromJust "action not defined" (M.lookup k m)

--tofunc map x = m \$ x

guardFromJust _   (Just x) = x
guardFromJust err Nothing  = error err

--auxialiary
funcFromList :: (Eq a) => [(a,b)] -> a -> b
funcFromList assoc x = guardFromJust "function not defined" (lookup x assoc)

--concrete examples
nothing = L Unit
just = R . Id
dfa1 :: LTS Char (Unit :+: Id) Int
dfa1 = M.fromList [ ('a', funcFromList [ (1,just 2)
                                       , (2,nothing)
                                       , (3,nothing)
                                       ])
                  , ('b', funcFromList [ (1,just 3)
                                       , (2,just 3)
                                       , (3,nothing)])
                  ]


l :: [a] -> List a
l = Lst . l2list

--stupid non generic way, just to check that fnctions are cprrectly defined
instance Crush [] where
   crush = foldr

lfa1 :: LTS Char [] Int
lfa1 = M.fromList [ ('a', funcFromList [ (1,[2,1])
                                       , (2,[] )
                                       , (3,[])
                                       ])
                  , ('b', funcFromList [ (1,[3])
                                       , (2,[1,3])
                                       , (3,[])])
                  ]


lfa2 :: LTS Char [] Int

lfa2 = M.fromList [ ('a', funcFromList [ (1,[2,4])
                                       , (3,[2,4])
                                       , (2,[])
                                       , (4,[])
                                       ])
                  , ('b', funcFromList [ (1,[2,4])
                                       , (3,[2,4])
                                       , (2,[])
                                       , (4,[])
                                       ])
                  ]

lfa3 = M.fromList [ ('a', funcFromList [ (1,[4,2])
                                       , (3,[2,4])
                                       , (2,[])
                                       , (4,[])
                                       ])
                  , ('b', funcFromList [ (1,[2,4])
                                       , (3,[2,4])
                                       , (2,[1])
                                       , (4,[])
                                       ])
                  ]

-- there are problems in encoding representations.
--let us try lists


bisimilar :: (Show s, Eq s, Ord a) => LTS a [] s -> s -> s -> Bool
bisimilar delta p q = runReader (bisim delta p q) [ ]

--horrible code! 
-- not yet correct..
alphabet = M.keys
bisim :: (Show s, Eq s, Ord a) => LTS a [] s -> s -> s -> Reader [(s,s)] Bool
bisim delta p q = do cycle <- asks (elem (p,q))
                     if p==q || cycle 
                       then return True
                       else liftM and $ mapM (bisimBy delta p q) (alphabet delta)

bisimBy :: (Show s, Eq s, Ord a) => LTS a [] s -> s -> s -> a -> Reader [(s,s)] Bool
bisimBy delta p q a = let p' = (delta \$ a) p
                          q' = (delta \$ a) q
                      in local ((p,q):) $
                          liftM (maybe False and)  $ zipWithM' (bisim delta) p' q' 


zipWithM' :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m (Maybe [c])
zipWithM' f [] [] = return $ Just []
zipWithM' f (a:as) (b:bs) = do xs <- zipWithM' f as bs 
                               case xs of Nothing-> return Nothing
                                          Just ys -> do 
                                                        r <- f a b
                                                        return $ Just(r : ys)
                                                        --liftM (maybe Nothing ( undefined :)) (zipWithM' f as bs)
zipWithM' f _ _ = return Nothing




type FList a = Unit :+: K a :*: Id
type List' a = Fix (FList a)

newtype List a = Lst (List' a)

l2list :: [a] -> List' a
l2list [] =     In $ L Unit
l2list (x:xs) = In $ R (K x :*: Id (l2list xs))

list2l :: List' a -> [a]
list2l (In (L Unit)) = []
list2l (In (R (K x :*: Id s))) = x : list2l s


instance Regular [a] where
    type PF [a] = Unit :+: K a :*: Id
    from [] = L Unit
    from (x:xs) = R (K x :*: Id xs)
    to (L Unit) = []
    to (R (K x :*: Id xs)) = x:xs



