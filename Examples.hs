module Examples where
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as M


-- Deterministic Finite Automata
-- The automata should be complete.
-- mapping from input type 'a' to function from state to state.
type DFA a s = Map a (s -> s) 
-- Non deterministic Finite Automata
-- mapping from input type 'a' to function from state to state(s).
type NFA a s = Map a (s -> Set s) 

-- mapping from input type 'a' to function from state to state(s),
--  which are ordered and not unique.
type LFA a s = Map a (s -> [s]) 

--examples of DFA's

-- dfa with chars as input, and state that are numbered.
-- E: alphabet: {a}
-- Q: states: {0,1}
-- q0: beginstate: 0
-- F: endstate: 0
-- recognizes: even ammount of 'a's
{-
==> [0] (a)==> 1 (a) ==> [0]
-}

-- this one is complete
cdfa1 :: DFA Char Int
cdfa1 = M.insert 'a' trans M.empty
  where
  -- transaction function.
  trans 0 = 1
  trans 1 = 0


-- testing functions.
run_dfa1 input = runCDFA cdfa1 0 input
run_dfa1_0 = run_dfa1 "aa"


--- run (multiple transitions)
-- run complete DFA
runCDFA :: (Ord a) => DFA a s -- automata
                   -> s       -- begin state
                   -> [a]     -- input
                   -> s       -- end state
runCDFA dfa s as = case as of
  []   -> s
  a:as -> runCDFA dfa (fromJust (M.lookup a dfa) $ s) as
  -- safe use of fromJust, the automaton is complete

-- run DFA
runDFA :: (Ord a) => DFA a s -- automata
                  -> s       -- begin state
                  -> [a]     -- input
                  -> Maybe s -- possible end state
runDFA dfa s as = case as of
  []   -> Just s
  a:as -> M.lookup a dfa >>= \q ->
          runDFA dfa (q s) as

-- run NFA
runNFA :: (Ord a, Ord s) => NFA a s  -- automata
                         -> s        -- begin state
                         -> [a]      -- input
                         -> Set s  -- end state(s), possible empty.
runNFA nfa s as = case as of
  []   -> S.insert s S.empty
  a:as -> case M.lookup a nfa of
    Nothing -> S.empty -- incomplete automaton, no transition
    Just q  -> let ss = S.map (flip (runNFA nfa) $ as) (q s)
               in  S.fold S.union S.empty ss

runLFA :: (Ord a) => LFA a s -- automata
                  -> s       -- begin state
                  -> [a]     -- input
                  -> [s]     -- end state(s), possible empty.
runLFA lfa s as = case as of
  []   -> [s]
  a:as -> case M.lookup a lfa of
    Nothing -> [] -- incomplete automaton, no transition
    Just q  -> q s >>= \s' ->
               runLFA lfa s' as

-- product (recognizes the intersection of the languages)

prodDFA :: (Ord a) => DFA a s1 -> DFA a s2 -> DFA a (s1, s2)
prodDFA d1 d2 = prod trans $ M.keys d1 -- d2 would have been fine as well
  where
    trans k = M.lookup k d1 >>= \q1 ->
              M.lookup k d2 >>= \q2 ->
              return $ newQ q1 q2
    newQ q1 q2 = \(s1,s2) -> (q1 s1, q2 s2)

prodNFA :: (Ord a, Ord s1, Ord s2) => NFA a s1 -> NFA a s2 -> NFA a (s1,s2)
prodNFA n1 n2 = prod trans $ M.keys n1
  where
    trans k = M.lookup k n1 >>= \q1 ->
              M.lookup k n2 >>= \q2 ->
              return $ \(s1, s2) -> S.fromList $ (newQ q1 q2) (s1, s2)
    newQ q1 q2 = \(s1, s2) ->
                 S.toList (q1 s1) >>= \s1' ->
                 S.toList (q2 s2) >>= \s2' ->
                 return (s1', s2')

prodLFA :: (Ord a) => LFA a s1 -> LFA a s2 -> LFA a (s1,s2)
prodLFA d1 d2 = prod trans $ M.keys d1
  where
    trans k = M.lookup k d1 >>= \q1 ->
              M.lookup k d2 >>= \q2 ->
              return $ newQ q1 q2
    newQ q1 q2 = \(s1, s2) ->
                 q1 s1 >>= \s1' ->
                 q2 s2 >>= \s2' ->
                 return (s1', s2')

prod t []     = M.empty
prod t (k:ks) = case t k of
  Nothing -> prod t ks
  Just q  -> M.singleton k q `M.union` prod t ks

--bissimilarity

(|>) :: (Ord a) => Map a f -> a -> f 

aut |> a = fromJust (M.lookup a aut)

bisimilarDFA :: (Ord a,Eq s) => DFA a s -> s -> s -> Bool
bisimilarDFA d p q = bisimilarDFA' d p q []

bisimilarDFA' d p q visited 
     | (p,q) `elem` visited = True
     | (q,p) `elem` visited = True
     | otherwise = let al = M.keys d
                   in and [bisimilarDFA' d ((d|>a) p) ((d|>a) q) ((p,q):visited) | a <- al]
--wrong

--problem here : need to check if already visited to stop recursion.
--dont know if there is a better way.
                        

bisimilarNFA :: (Ord a, Ord s) => NFA a s -> s -> s -> Bool
bisimilarNFA d p q = bisimilarNFA' d p q []

bisimilarNFA' d p q visited
    | (p,q) `elem` visited = True
    | (q,p) `elem` visited = True
    | otherwise  = let al = M.keys d
                   in  and (map check al)
    where check a = let ps = (d |> a) p
                        qs = (d |> a) q 
                    in setall (\p'->setany (\q'-> bisimilarNFA' d p' q' ((p,q):visited)) qs) ps 
                       &&
                       setall (\q'->setany (\p'-> bisimilarNFA' d p' q' ((p,q):visited)) ps) qs


setall p = S.fold (&&) True  . S.map p
setany p = S.fold (||) False . S.map p

bisimilarLFA :: (Ord a , Eq s) => LFA a s -> s -> s -> Bool
bisimilarLFA d p q = bisimilarLFA' d p q []

bisimilarLFA' :: (Ord a, Eq s) => LFA a s -> s -> s -> [(s,s)] -> Bool
bisimilarLFA' d p q visited 
    | (p,q) `elem` visited = True
    | (q,p) `elem` visited = True
    | otherwise  = let al = M.keys d
                   in  and (map check al)
    where check a = let ps = (d |> a) p
                        qs = (d |> a) q 
                    in all (\p'->any (\q'-> bisimilarLFA' d p' q' ((p,q):visited)) qs) ps 
                       &&
                       all (\q'->any (\p'-> bisimilarLFA' d p' q' ((p,q):visited)) ps) qs

