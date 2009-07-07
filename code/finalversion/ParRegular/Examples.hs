{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE TypeFamilies#-}
module ParRegular.Examples where

import ParRegular.Representations
import ParRegular.Base

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


instance ParRegular Maybe where
  type  PBF Maybe    = Unit :+: Par
  from  Nothing      = L Unit
  from  (Just x)     = R (Par x)
  to    (L Unit)     = Nothing
  to    (R (Par x))  = Just x


tree = (Fork (Leaf (Just (Cons 3 Nil))) (Fork (Leaf Nothing) (Leaf (Just (Cons 2 (Cons 5 (Cons 3 Nil)))))))

-- *ParRegular.Examples> flatten tree
-- [Just (Cons 3 Nil),Nothing,Just (Cons 2 (Cons 5 (Cons 3 Nil)))]
-- *ParRegular.Examples> flatten (Comp tree)
-- [Cons 3 Nil,Cons 2 (Cons 5 (Cons 3 Nil))]
-- *ParRegular.Examples> flatten (Comp (Comp tree))
-- [3,2,5,3]



