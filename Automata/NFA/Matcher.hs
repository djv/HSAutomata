module Automata.NFA.Matcher where
    
import Data.Set
import Test.QuickCheck.Gen

import Automata.NFA.Datatype (NFA(..), next, epsilonClosure, trie, mapNFAToNFA)
import Automata.Utils (setConcat, fix)


-- | An NFA 'matches' a finite list of input
-- iff after following all possible transitions for each item in the list,
-- one of the resulting states is an accepting state.
matches :: Ord stateLabel =>    
           NFA stateLabel a  -- ^ NFA
        -> [a]               -- ^ Input sequence. Must be finite.
        -> Bool              -- ^ True iff the NFA matches the input.
matches nfa input = matches' (epsilonClosure nfa (singleton (start nfa)))
    where matches' states
           = any (accepting nfa) (elems finalStates)
             where finalStates = foldl ((epsilonClosure nfa . ) . (next nfa)) states input

prop_accepts = do
    inp <- resize 20 $ listOf1 $ listOf1 $ choose ('a', 'c')
    let tr = trie inp
    return $ all (matches $ mapNFAToNFA tr) inp

prop_notAccepts = do
    inp <- resize 20 $ listOf1 $ listOf1 $ choose ('a', 'c')
    let tr = trie inp
    notInp <- listOf1 $ (listOf1 $ choose ('a', 'c')) `suchThat` (not . (flip elem) inp)
    return $ not $ any (matches $ mapNFAToNFA tr) notInp