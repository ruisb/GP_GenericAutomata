{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE TypeFamilies#-}
module TreeAutomata.Example where

import TreeAutomata.Representation
import TreeAutomata.Operations

import Regular.Representations
import Regular.Base


-- | The tree-like structure of the valid words
data Expr  =  Zero
	   |  Suc Expr
	   |  Plus Expr Expr

	   |  F
	   |  T
	   |  And Expr Expr
           |  Not Expr

           |  Eq  Expr Expr
	   |  IfThenElse Expr Expr Expr
        deriving (Eq, Show)

-- | Generic Representation of this regular type
instance Regular Expr where
  type PF Expr = Unit :+: Id :+: Id :*: Id :+: Unit :+: Unit :+: Id :*: Id :+: Id :+: Id :*: Id :+: Id :*: Id :*: Id
  from Zero                = L Unit
  from (Suc x)             = R (L (Id x))
  from (Plus x y)          = R (R (L (Id x :*: Id y)))
  from F                   = R (R (R (L Unit)))
  from T                   = R (R (R (R (L Unit)))) 
  from (And x y)           = R (R (R (R (R (L (Id x :*: Id y))))))
  from (Not x)             = R (R (R (R (R (R (L (Id x)))))))
  from (Eq x y)            = R (R (R (R (R (R (R (L (Id x :*: Id y))))))))
  from (IfThenElse x y z)  = R (R (R (R (R (R (R (R (Id x :*: Id y :*: Id z))))))))  
  to   (L Unit)                                                  =  Zero                     
  to   (R (L (Id x)))                                            =  (Suc x)               
  to   (R (R (L (Id x :*: Id y))))                               =  (Plus x y)            
  to   (R (R (R (L Unit))))                                      =  F                     
  to   (R (R (R (R (L Unit)))))                                  =  T                      
  to   (R (R (R (R (R (L (Id x :*: Id y)))))))                   =  (And x y)            
  to   (R (R (R (R (R (R (L (Id x))))))))                        =  (Not x)             
  to   (R (R (R (R (R (R (R (L (Id x :*: Id y)))))))))           =  (Eq x y)            
  to   (R (R (R (R (R (R (R (R (Id x :*: Id y :*: Id z)))))))))  =  (IfThenElse x y z)  


-- | The example automaton (states, transition function and the automaton itself)

data Types = N | B

-- (this way of writing is a bit cumbersome, we should think of a better one)
texample :: TreeAutTrans Expr Types           
texample N  =  [ L Unit                                                   -- Zero
               , R (L (Id N))                                             -- Suc
               , R (R (L (Id N :*: Id N)))                                -- Plus
               ,  R (R (R (R (R (R (R (R (Id B :*: Id N :*: Id N))))))))  -- IfThenElse - Nat
               ]
texample B  =  [  R (R (R (L Unit)))                                      -- False
               ,  R (R (R (R (L Unit))))                                  -- True
               ,  R (R (R (R (R (L (Id B :*: Id B))))))                   -- And
               ,  R (R (R (R (R (R (L (Id B)))))))                        -- Not
               ,  R (R (R (R (R (R (R (L (Id B :*: Id B))))))))           -- Eq - Bool
               ,  R (R (R (R (R (R (R (L (Id N :*: Id N))))))))           -- Eq - Nat
               ,  R (R (R (R (R (R (R (R (Id B :*: Id B :*: Id B))))))))  -- IfThenElse - Bool
               ]

example :: TreeAutomaton Expr Types
example = TA {initials = [N], delta = texample}

-- | two sample expressions: the first is accepted, the second not

expr1 = IfThenElse (Eq Zero Zero) 
                   (Suc Zero)
                   (Plus Zero Zero)

expr2 = IfThenElse (Eq Zero Zero) 
                   (Suc Zero)
                   (Eq Zero Zero)


