module Generic where

import Regular2.Base

import qualified Data.Map as M
import qualified Data.Set as S

-- Transition system
type TS f s = s -> f s

-- Labeled transition system
type LTS a f s = M.Map a (TS f s)

data Star f s = End s
              | Step (f (Star f s))

toSet :: (Crush f, Ord a) => f a -> S.Set a
toSet fa = crush S.insert S.empty fa

run1 :: (Ord a, Ord s, Crush f, Functor f) =>
        LTS a f s -> [a] -> s -> S.Set s
run1 lts xs s = case xs of
  []   -> S.singleton s
  y:ys -> case M.lookup y lts of
    Nothing -> error "Incomplete automaton!" -- incomplete automaton, no transition
    Just ts -> let fs = fmap (run1 lts ys) $ ts s
               in  S.fold S.union S.empty $ toSet fs

run2 :: (Ord a, Ord s, Functor f) =>
        LTS a f s -> [a] -> s -> Star f s
run2 lts xs s = case xs of
  []   -> End s
  y:ys -> case M.lookup y lts of
    Nothing -> error "Incomplete automaton!"
    Just ts -> Step $ fmap (run2 lts ys) $ ts s

run3 :: (Ord a, Ord s, Monad f) => LTS a f s -> [a] -> s -> f s
run3 lts xs s = case xs of
  []   -> return s
  y:ys -> case M.lookup y lts of
    Nothing -> fail "Incomplete automaton!"
    Just ts -> ts s >>= \s' -> run3 lts ys s'
