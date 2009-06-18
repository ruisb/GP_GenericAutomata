--------------------------------------------------------------------------------
{-
Some functions cannot be written with the format of automata in examples. For
example the isComplete function.
Also it's maybe nicer to store all information (beginstate, endstate) in the
same data constr. But of course this makes the generalistation for generic
programming more complex.

-}
--------------------------------------------------------------------------------

module Examples2 where
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)

import Examples (runDFA)

data DFA' a s = DFA (Set s)           -- Q: all the states
                    (Set a)           -- E (Sigma): the alfabet.
                    (Map a (Map s s)) -- d (delta): transaction mapping
                    s                 -- q0: start state
                    s                 -- F:  end state

data NFA' a s = NFA (Set s) -- Q: all the states
                    (Set a) -- E (Sigma): the alfabet.
                    (Map a (Map s (Set s))) -- d (delta): transaction mapping
                    s         -- q0: start state
                    (Set s) -- F:  end state(s)


-- complete: every state accepts all input.
isComplete :: (Ord a, Ord s) => DFA' a s -> Bool
isComplete (DFA states alphabet trans start end)
 = all (\a -> case M.lookup a trans of
                   Nothing -> False
                   Just t  -> all (\s -> M.member s t) (S.toList states)
                   )
        (S.toList alphabet)

--examples of DFA's

-- dfa with chars as input, and state that are numbered.
-- E: alphabet: {a}
-- Q: states: {0,1}
-- q0: beginstate: 0
-- F: endstate: 0
-- recognizes: even ammount of 'a's
cdfa1' :: DFA' Char Int
cdfa1' = DFA states alphabet trans start end
  where
  states = S.insert 1 (S.singleton 0) -- {0, 1}
  alphabet = S.singleton 'a'          -- {a}
  start = 0                           -- 0
  end   = 0                           -- 0
  trans = M.singleton 'a' transa
    where
    transa = M.insert 0 1 (M.singleton 1 0)

-- tests
tComp = isComplete cdfa1'

-- from map to function, for the runDFA func.
mapping2func :: (Ord a) => Map a b -> (a -> b)
mapping2func mapping = (\k -> case M.lookup k mapping of
                              Just a  -> a
                              Nothing -> undefined)


-- accept the DFA this input?
acceptDFA :: (Ord a, Ord s) =>  DFA' a s -> [a] -> Bool
acceptDFA (DFA states alphabet trans start end) input
  = case runDFA (M.map mapping2func trans) start input of
         Just s  -> s == end
         Nothing -> False

-- tests
tAccept0 = acceptDFA cdfa1' "aa"
tAccept1 = acceptDFA cdfa1' "aaa"