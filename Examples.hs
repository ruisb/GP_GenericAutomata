module Examples where
import qualified Data.Set as S
import Data.Maybe (fromJust)
import qualified Data.Map as M

type DFA a s = M.Map a (s -> s    ) -- deterministic, complete
type NFA a s = M.Map a (s -> S.Set s) -- non deterministic
type LFA a s = M.Map a (s -> [s]  ) -- ordered non deterministic


-- run (multiple transitions)

runCDFA :: (Ord a) => DFA a s -> s -> [a] -> s
runCDFA dfa s as = case as of
  []   -> s
  a:as -> runCDFA dfa (fromJust (M.lookup a dfa) $ s) as
  -- safe use of fromJust, the automaton is complete

runDFA :: (Ord a) => DFA a s -> s -> [a] -> Maybe s
runDFA dfa s as = case as of
  []   -> Just s
  a:as -> M.lookup a dfa >>= \q ->
          runDFA dfa (q s) as

runNFA :: (Ord a, Ord s) => NFA a s -> s -> [a] -> S.Set s
runNFA nfa s as = case as of
  []   -> S.insert s S.empty
  a:as -> case M.lookup a nfa of
    Nothing -> S.empty -- incomplete automaton, no transition
    Just q  -> let ss = S.map (flip (runNFA nfa) $ as) (q s)
               in  S.fold S.union S.empty ss

runLFA :: (Ord a) => LFA a s -> s -> [a] -> [s]
runLFA lfa s as = case as of
  []   -> [s]
  a:as -> case M.lookup a lfa of
    Nothing -> [] -- incomplete automaton, no transition
    Just q  -> q s >>= \s' ->
               runLFA lfa s' as

-- product
prodDFA :: DFA a s1 -> DFA a s2 -> DFA a (s1,s2)
prodDFA = undefined

prodNFA :: NFA a s1 -> NFA a s2 -> NFA a (s1,s2)
prodNFA = undefined

prodLFA :: LFA a s1 -> LFA a s2 -> LFA a (s1,s2)
prodLFA = undefined

--bissimilarity
bisimilarDFA :: DFA a s -> s -> s -> Bool
bisimilarDFA = undefined
bisimilarNFA :: NFA a s -> s -> s -> Bool
bisimilarNFA d = undefined

bisimilarLFA :: LFA a s -> s -> s -> Bool
bisimilarLFA d = undefined



