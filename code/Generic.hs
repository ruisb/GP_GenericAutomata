{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-#LANGUAGE FlexibleInstances#-}
module Generic where

import Regular2.Representations
import Regular2.Base

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Control.Arrow
import Control.Monad.State
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

-- there are problems in encoding representations.
--let us try lists


--i:bisim :: LTS a f s -> (s,s) -> Bool
--bisim delta (p,q) = evalState (bisim' delta (p,q)) [ []]

--horrible code! 
-- not yet correct..
---bisim' :: LTS a f s -> s -> s -> State [(s,s)] Bool
---bisim' delta (p,q) = do b <- gets (lookup (p,q))
---                        if isJust
---                         then return True
---                         else M.foldM (&&) False M.map (\a -> zipWithM (bisim' delta) (delta \$ a p) (delta \$ a q)) 
---


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



