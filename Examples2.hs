--------------------------------------------------------------------------------
{-
Some functions cannot be written with the format of automata in examples. For
example the isComplete function or the complement function.
Also it's maybe nicer to store all information (beginstate, acceptingstate) in the
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

import Examples (runDFA, runNFA)

-- Deterministic Finite Automata
-- The automata should be complete.
-- mapping from input type 'a' to function from state to state.
data DFA' a s = DFA (Set s)           -- Q: all the states
                    (Set a)           -- E (Sigma): the alfabet.
                    (Map a (Map s s)) -- d (delta): transaction mapping
                    s                 -- q0: start state
                    (Set s)           -- F:  accepting states
                    
-- Non deterministic Finite Automata
-- mapping from input type 'a' to function from state to state(s).
data NFA' a s = NFA (Set s)                 -- Q: all the states
                    (Set a)                 -- E (Sigma): the alfabet.
                    (Map a (Map s (Set s))) -- d (delta): transaction mapping
                    s                       -- q0: start state
                    (Set s)                 -- F:  accepting states

-- mapping from input type 'a' to function from state to state(s),
--  which are ordered and not unique.
data LFA' a s = LFA (Set s)             -- Q: all the states
                    (Set a)             -- E (Sigma): the alfabet.
                    (Map a (Map s [s])) -- d (delta): transaction mapping
                    s                   -- q0: start state
                    (Set s)             -- F:  accepting states


-- create a complement automata that accepts the complement language.
-- the complement of Automata can be obtained by swapping its accepting states
-- with its non-accepting states.
complementDFA :: (Ord s) =>  DFA' a s -> DFA' a s
complementDFA (DFA states alphabet trans start accepting)
 = (DFA states alphabet trans start nonaccepting)
   where
   nonaccepting = states S.\\ accepting
   
-- idem for NFA
complementNFA :: (Ord s) =>  NFA' a s -> NFA' a s
complementNFA (NFA states alphabet trans start accepting)
 = (NFA states alphabet trans start nonaccepting)
   where
   nonaccepting = states S.\\ accepting

-- complete: every state accepts all input.
isComplete :: (Ord a, Ord s) => DFA' a s -> Bool
isComplete (DFA states alphabet trans start accepting)
 = all (\a -> case M.lookup a trans of
                   Nothing -> False
                   Just t  -> all (\s -> M.member s t) (S.toList states)
                   )
        (S.toList alphabet)

-- examples of DFA's
-- dfa with chars as input, and state that are numbered.
-- E: alphabet: {a}
-- Q: states: {0,1}
-- q0: beginstate: 0
-- F: accepting state: {0}
-- recognizes: even ammount of 'a's
cdfa1' :: DFA' Char Int
cdfa1' = DFA states alphabet trans start accepting
  where
  states     = S.insert 1 (S.singleton 0) -- {0, 1}
  alphabet   = S.singleton 'a'            -- {a}
  start      = 0                          -- 0
  accepting  = S.singleton 0              -- {0}
  trans      = M.singleton 'a' transa
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
acceptDFA (DFA states alphabet trans start accepting) input
  = case runDFA (M.map mapping2func trans) start input of
         Just s  -> S.member s accepting -- should be in accepting states
         Nothing -> False
         
-- accept the DFA this input?
acceptNFA :: (Ord a, Ord s) =>  NFA' a s -> [a] -> Bool
acceptNFA (NFA states alphabet trans start accepting) input
  = case S.toList $ runNFA (M.map mapping2func trans) start input of
          -- CHANGE with acceptDFA:
          -- at least one should be member of accepting states.
         [] -> False
         xs -> any (\s -> S.member s accepting) xs
         

-- tests
prop_accept0 = acceptDFA cdfa1' "aa" == True
prop_accept1 = acceptDFA cdfa1' "aaa" == False
-- complement tests
prop_accept2 = acceptDFA (complementDFA cdfa1') "aa" == False
prop_accept3 = acceptDFA (complementDFA cdfa1') "aaa" == True

