{-#LANGUAGE TypeOperators#-}
module ParRegular.Data.FiniteFunctions where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad

type (:->) = Map 

infixl 9 \$
(\$) :: (Ord a) => (a :-> b) -> a -> b
f \$ x = guardFromJust "finite function not defined" (M.lookup x f)
guardFromJust _   (Just x) = x
guardFromJust err Nothing  = error err

infixr 9 \.
(\.) :: (Ord a) => (b -> c) -> (a :-> b) -> (a :-> c)
g \. f = M.map g f


ff_sequence  :: (Ord a, Monad m) => (a :-> m b) -> m (a :-> b)
ff_sequence =  liftM M.fromList . sequence . map (\(a,mb) -> liftM ((,) a) mb) . M.toList 

domain :: (Ord a) => (a :-> b) -> [a]
domain = M.keys

range   :: (Ord a) => (a :-> b) -> [b] 
range   = M.elems

fromFunction :: (Ord a) => [a] -> (a -> b) -> a :-> b
fromFunction d f = M.fromList (map (\x->(x,f x)) d)

ff_fromList :: (Ord a) => [(a,b)] -> a :-> b
ff_fromList = M.fromList

