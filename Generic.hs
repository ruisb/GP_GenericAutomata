module Generic where

import Regular2.Base

import qualified Data.Map as M
import qualified Data.Set as S

-- Transition system
type TS f s = s -> f s

-- Labeled transition system
type LTS a f s = M.Map a (TS f s)

toSet :: (Crush f, Ord a) => f a -> S.Set a
toSet fa = crush S.insert S.empty fa

run1 :: (Ord a, Ord s, Crush f, Functor f) =>
        LTS a f s -> [a] -> s -> S.Set s
run1 lts xs s = case xs of
  []   -> S.singleton s
  x:xs -> case M.lookup x lts of
    Nothing -> S.empty -- incomplete automaton, no transition
    Just ts -> let fs = fmap (run1 lts xs) $ ts s
               in  S.fold S.union S.empty $ toSet fs
