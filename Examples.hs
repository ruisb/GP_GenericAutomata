module Examples where
import Data.Set

type DFA a s = s -> a -> s     -- deterministic
type NFA a s = s -> a -> Set s -- non deterministic
type LFA a s = s -> a -> [s]   -- ordered non deterministic


-- run (multiple transitions)

runDFA :: DFA a s -> s -> [a] -> s
runDFA = undefined

runNFA :: NFA a s -> s -> [a] -> Set s
runNFA = undefined

runLFA :: LFA a s -> s -> [a] -> [s]
runLFA = undefined


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
bisimilarNFA = undefined
bisimilarLFA :: LFA a s -> s -> s -> Bool
bisimilarLFA = undefined



