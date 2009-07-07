{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE TypeFamilies #-}
module TreeAutomata.Representation where 
import Regular.Representations
import Regular.Base
import Control.Monad

-- this sohuld be in the generic library...
type CoAlgebra     f r = r -> f r
type CoAlgebraM m  f r = r -> m (f r)

unfold      :: (Regular t) => CoAlgebra (PF t) r -> r -> t
unfold g    = to . fmap (unfold g) . g 

unfoldM    :: (GMap (PF t), Regular t, Monad m) => CoAlgebraM m (PF t) r -> r -> m t
--unfoldM g  = liftM to . join . liftM (fmapM (unfoldM g)) . g
unfoldM g =  liftM to . fmapM (unfoldM g) <=< g


-- | Representation of the Transition Function of a Tree Automaton
type TreeAutTrans t s = CoAlgebraM [] (PF t) s

-- | Representation of a ND TopDown Tree Automaton
data TreeAutomaton t s = TA { delta :: TreeAutTrans t s, initials :: [s] }


