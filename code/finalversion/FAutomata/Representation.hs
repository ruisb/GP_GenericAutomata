{-#LANGUAGE TypeOperators#-}
module FAutomata.Representation where

import ParRegular.Data.FiniteFunctions

-- Transition system
type TS f s = s -> f s

-- Labeled transition system
type LTS f a s = a :-> TS f s

