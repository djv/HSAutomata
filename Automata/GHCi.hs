-- | Includes two high level functions for manual testing in GHCi.
module Automata.GHCi where

import Automata.DFA.Datatype as DFA
import Automata.DFA.MyDFA as MDFA
import Automata.NFA.Datatype as NFA
import qualified Automata.NFA.Graphviz as NGraphviz
import qualified Automata.DFA.Graphviz as DGraphviz

import qualified Automata.DFA.Matcher as DMatcher
import qualified Automata.NFA.Matcher as NMatcher

import Automata.DFA.Minimize

import Automata.RegularExpression
import Automata.RegularExpression.Parser
import Automata.Conversions
import Automata.System
import Automata.Utils
import Test.QuickCheck.Gen
import Test.QuickCheck
import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Control.Arrow
import qualified Data.ByteString.Char8 as B

-- @regexOnscreen@ should be pronounced as if Captain Picard were saying it.
regexOnscreenN = previewDot . NGraphviz.toGraphviz . NFA.removeUnreachableStates . regularExpressionToNFA . eParse
regexOnscreenD = previewDot . DGraphviz.toGraphviz . minimize . mapNFAToMapDFA . regularExpressionToNFA . eParse

nfaOnscreen x = previewDot . NGraphviz.toGraphviz . NFA.removeUnreachableStates $ x
dfaOnscreen x = previewDot . DGraphviz.toGraphviz $ x
mdfaOnscreen x = previewDot . DGraphviz.mtoGraphviz $ x

type Widget = String
type Link = String
anywidget = ""
anystring = ""
anylink = ""

data UserAction = Click Widget
                | Type String
                | FollowLink Link
                deriving (Show)
instance Eq UserAction where
    Click _ == Click _ = True
    Type _ == Type _ = True
    FollowLink _ == FollowLink _ = True
    _ == _ = False            
    
instance Ord UserAction where
    Click _ <= Click _ = False
    Click _ <= _ = True
    Type _ <= FollowLink _ = True
    Type _ <= _ = False
    FollowLink _ <= _ = False

charToAction 'c' = Click anywidget
charToAction 't' = Type anystring
charToAction 'f' = FollowLink anylink

typetype = fmap charToAction (eParse "tt")
typeClicksThenLeave = fmap charToAction (eParse "tc*f")
match r = NMatcher.matches (mapNFAToNFA (regularExpressionToNFA r))

genInpList :: Gen [String]
genInpList = listOf1 $ listOf1 $ choose ('a', 'c')

prop_accepts = do
    inp <- genInpList
    let dict = buildDictionary $ map B.pack $ sort inp
    return $ all (DMatcher.matches (myDFAToDFA dict)) inp

prop_notAccepts = do
    inp <- genInpList
    let dict = buildDictionary $ map B.pack $ sort inp
    notInp <- genInpList `suchThat` (Data.List.null . intersect inp)
    return $ not $ Data.List.any (DMatcher.matches $ myDFAToDFA dict) notInp

prop_acceptsTrie = do
    inp <- genInpList
    let dict = buildTrie $ map B.pack $ sort inp
    return $ all (DMatcher.matches (myDFAToDFA dict)) inp

prop_notAcceptsTrie = do
    inp <- genInpList
    let dict = buildTrie $ map B.pack $ sort inp
    notInp <- genInpList `suchThat` (Data.List.null . intersect inp)
    return $ not $ Data.List.any (DMatcher.matches $ myDFAToDFA dict) notInp

prop_minStates = do
    inp <- genInpList
    let dictNum = MDFA.stateSize $ buildDictionary $ map B.pack $ inp
    let minNum = Set.size $ DFA.states $ minimize $ mapNFAToMapDFA $ trie $ inp
    return $ traceShow (minNum, dictNum) $ True--minNum == dictNum

checkSize = do
        inpStr <- B.readFile "test"
        let inp = B.lines inpStr :: [B.ByteString]
        let tests = [(500,1702), (5000,12709)]
        return $ map (uncurry (==) . first (stateSize . buildDictionary . (flip take) inp)) tests

l = ["a","a","aaaaacb","aaaabab","aab","aabbacccbab","aaccaccbbaaa","ab","ab","abbaabbbbca","abbcaaacbcaab","abcaaa","abcaabcaba","abccb","acbaaa","acbcbbcbbc","acbccac","acccaccbcbb","accccaca","b","b","b","b","ba","baab","baabbbbcbccac","baabcabab","bababbacba","bbac","bbaca","bbba","bbbc","bbbcc","bbccb","bc","bcaa","bcacbaaabbca","bcbaaacbabaccacbcbcb","bcbacba","bcbbbcb","bcbcbaaabcbc","bccacabbacaa","c","caabbacbbbbc","caabbc","caabbcbaa","cab","cabaacabb","cabbabcccc","cabc","cacbbccbcaaaabacaa","cacca","cb","cbabbabbcb","cbacabbbaaa","cbacacacaacccabb","cbbbbaa","cbbc","cbbca","cc","ccab","ccacbbaa","ccbaccbbaab","ccbcaaabbbbcacc","ccc","ccc","cccacccaaaa"]
t n = buildTrie $ map B.pack $ take n l
