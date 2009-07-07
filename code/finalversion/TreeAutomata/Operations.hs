{-#LANGUAGE FlexibleContexts#-}
module TreeAutomata.Operations where

import TreeAutomata.Representation

import Regular.Representations
import Regular.Base

import Control.Monad
import Control.Arrow
import Data.Maybe

-- | The whole language recognized by an automaton
language :: (Regular t, GMap (PF t)) =>  TreeAutomaton t s -> [t]
language aut = unfoldM (delta aut) =<< initials aut

-- | Check whether a (finite) tree is recognized by the automaton (DFS-like)
accept ::  (Regular t, Zip (PF t)) =>TreeAutomaton t s -> t -> Bool
accept aut t = (isJust . msum)  [accept' (delta aut) t i | i <- initials aut]

accept' :: (Regular t, Zip (PF t)) => TreeAutTrans t s -> t -> s -> Maybe ()
accept' delta t =  msum . map ( liftM (!) . fzipM (accept' delta) (from t)) . delta

(!) = const ()

-- | Run a (rewriting) step of the automaton (BFS-like).
-- useful to deal with partial runs in infinite trees
runStep :: (Regular t, Zip (PF t)) => TreeAutTrans t s -> t -> s -> Maybe [PF t (t,s)]
runStep delta t s = sequence [fzip (,) (from t) m | m <- delta s]


-- | The automaton that recognizes the union of two tree languages
union :: (Regular t) => TreeAutomaton t s1 -> TreeAutomaton t s2 -> TreeAutomaton t (Either s1 s2)
union aut1 aut2 = TA { initials  = map Left (initials aut1) ++ map Right (initials aut2)
                     , delta     = map (fmap Left) . (delta aut1) ||| map (fmap Right) . (delta aut2)
                     }

-- | The automaton that recognizes the intersection of two tree languages
intersection :: (Regular t, Zip (PF t)) => TreeAutomaton t s1 -> TreeAutomaton t s2 -> TreeAutomaton t (s1,s2)
intersection aut1 aut2 = TA { initials = cartesian (initials aut1 , initials aut2)
                            , delta    = map fromJust . filter isJust . map  (uncurry (fzip (,))) . cartesian . (delta aut1 *** delta aut2)
                            }

cartesian :: ([a] , [b]) -> [(a,b)]
cartesian (a , b) = [(x,y) | x <- a, y <- b]


