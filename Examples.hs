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

bissimilarity
aut |> a = fromJust (M.lookup a aut)

bisimilarDFA :: (Eq s) => DFA a s -> s -> s -> Bool
bisimilarDFA d p q = bisimilarDFA' d p q []

bisimilarDFA' d p q visited 
     | (p,q) `elem` visited = True
     | (q,p) `elem` visited = True
     | otherwise = let al = M.keys d
                   in all[bisimilarDFA' d p' q' ((p,q):visited) | a <- al sp'<- (d |> a) p, q' <- (d |> a) q]
--wrong

--problem here : need to check if already visited to stop recursion.
--dont know if there is a better way.
                        

bisimilarNFA :: (Ord s) => NFA a s -> s -> s -> Bool
bisimilarNFA d p q = bisimilarNFA' d p q []

bisimilarNFA' d p q visited
    | (p,q) `elem` visited = True
    | (q,p) `elem` visited = True
    | otherwise  = let al = M.keys d
                       ps = (d |> d) p
                       qs = (d |> d) q 
                   in all (\p'->any (\q'->bisimilarNFA' d p' q' ((p,q):visited)) qs) ps 
                      &&
                      all (\q'->any (\p'->bisimilarNFA' d p' q' ((p,q):visited)) ps) qs

bisimilarLFA :: (Eq s) => LFA a s -> s -> s -> Bool
bisimilarLFA d p q = bisimilarLFA' d p q []

bisimilarLFA' d p q visited 
    | (p,q) `elem` visited = True
    | (q,p) `elem` visited = True
    | otherwise  = let al = M.keys d
                       ps = d |> d p
                       qs = d |> d q 
                   in setall (\p'->setany (\q'->bisimilarNFA' d p' q' ((p,q):visited)) qs) ps 
                      &&
                      setall (\q'->setany (\p'->bisimilarNFA' d p' q' ((p,q):visited)) ps) qs


setall p = S.fold (&&) True  . S.map p
setany p = S.fold (||) False . S.map p
