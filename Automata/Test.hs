-- | Test dispatcher.
-- 
-- Run with: 
-- $ ghc --make Automata/Test.hs -main-is Automata.Test.main
-- $ Automata/Test

module Automata.Test where
    
import Automata.DFA.Datatype as DFA
import Automata.NFA.Datatype as NFA
import qualified Automata.NFA.Graphviz as NGraphviz
import qualified Automata.DFA.Graphviz as DGraphviz

import qualified Automata.DFA.Matcher as DMatcher
import qualified Automata.NFA.Matcher as NMatcher

import Automata.DFA.LLVM
import Automata.DFA.Minimize

import Automata.RegularExpression
import Automata.RegularExpression.Parser
import Automata.Conversions
import Automata.System
import Automata.Utils


import Data.ByteString.Internal (c2w)
import System.Exit
import System.IO.Unsafe
import Test.QuickCheck

--main = mapM_ quickCheck tests

prop_quickCheck = label "QuickCheck is working" True
-- TODO Make into a test case: (Automata.NFA.Matcher.matches $ mapNFAToNFA $ optional $ mapDFAToMapNFA $ minimize $ mapNFAToMapDFA $ regularExpressionToNFA $ eParse "(ab)*a") "ab"
-- Should be False


tests = [prop_quickCheck]

------------------------------------------------------------
-- Tests on regular expressions and various transformations


basicMapNFA fn     = regularExpressionToNFA . fmap fn . eParse
reachableMapNFA fn = NFA.removeUnreachableStates . (basicMapNFA fn)
basicMapDFA  fn    = mapNFAToMapDFA . (basicMapNFA fn)
basicMapDFA' fn    = mapNFAToMapDFA . (reachableMapNFA fn)
minMapDFA fn       = minimize . (basicMapDFA fn)
minMapDFA' fn      = minimize . (basicMapDFA' fn)

[nfa1, nfa2]
    = map ((NMatcher.matches .) . (mapNFAToNFA .) . ($id)) 
          [ basicMapNFA
          , reachableMapNFA]
    
        
[dfa1, dfa2, dfa3, dfa4]
    = map ((DMatcher.matches .) . (mapDFAToDFA .) . ($id))
          [ basicMapDFA
          , basicMapDFA'
          , minMapDFA
          , minMapDFA']    

[llvm1, llvm2, llvm3, llvm4] = map ((unsafePerformIO .) . (compile .) . ($c2w))
                                   [ basicMapDFA
                                   , basicMapDFA'
                                   , minMapDFA
                                   , minMapDFA']

data Matcher =
    Matcher
    { matcherName :: String
    , fn :: (String -> String -> Bool)
    }
                                   
regexMatchers 
    = [ Matcher "Basic NFA" nfa1, 
        Matcher "Reachable-States NFA" nfa2, 
        Matcher "Basic DFA" dfa1, 
        Matcher "Reachable-States DFA" dfa2, 
        Matcher "Minimized Basic DFA" dfa3, 
        Matcher "Minimized Reachable DFA " dfa4, 
        Matcher "LLVM Basic DFA" llvm1, 
        Matcher "LLVM Reachable DFA" llvm2, 
        Matcher "LLVM Min Basic DFA" llvm3, 
        Matcher "LLVM Min Reachable DFA" llvm4]

data RegexTest = 
    RegexTest
    { regex :: String
    , shouldMatch :: [String]
    , shouldNotMatch :: [String]
    }
    
emptyTest = RegexTest "" [""] ["a"]
singletonTest = RegexTest "a" ["a"] ["", "b", "ab", "ba", "aba"]
kleeneTest = RegexTest "a*" ["", "a", "aa", "aaa"] ["b", "ab", "ba", "baa"]
catTest = RegexTest "ab" ["ab"] ["", "a", "b", "abc", "cab", "abab"]
altTest = RegexTest "a|b" ["a", "b"] ["", "ab", "ba", "c"]

regexTests 
    = [ emptyTest
      , singletonTest
      , kleeneTest
      , catTest
      , altTest
      ]
    

runRegexTest :: RegexTest -> Matcher -> [(Bool, String)]
runRegexTest (RegexTest r shoulds shouldNots) (Matcher name fn)
    = let matcher = fn r
      in (shoulds >>= (\should -> return (matcher should, name ++ " :: " ++ show r ++ " should match " ++ show should))) ++
         (shouldNots >>= (\shouldNot -> return (not $ matcher shouldNot, name ++ " :: " ++ show r ++ " should not match " ++ show shouldNot)))

runTests tests matchers 
    = do test <- tests
         matcher <- matchers
         runRegexTest test matcher
         

main = let results = map snd $ filter (not . fst) (runTests regexTests regexMatchers)
       in  do mapM_ putStrLn results
              if (length results > 0) 
                then do putStrLn $ (show (length results)) ++ " tests failed."
                        exitFailure 
                else do putStrLn "OK"
                        exitSuccess

-- nfa matches (or not) what the removeUnreachableStates matches
-- nfa matches (or not) what the mapNFAToMapDFA matches
-- dfa matches what minimize dfa matches
-- dfaToNFA $ minimize dfa matches what the dfa matches
-- each operation (singleton, catenation, alternation, optional, etc.) operates correctly
-- each operation operates correctly on an NFA that was looped through dfa minimization

