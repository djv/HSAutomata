-- | Includes two high level functions for manual testing in GHCi.
module Automata.GHCi where

import Automata.DFA.Datatype as DFA
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
import Data.Set
import Debug.Trace
import Control.Arrow

-- @regexOnscreen@ should be pronounced as if Captain Picard were saying it.
regexOnscreenN = previewDot . NGraphviz.toGraphviz . NFA.removeUnreachableStates . regularExpressionToNFA . eParse
regexOnscreenD = previewDot . DGraphviz.toGraphviz . minimize . mapNFAToMapDFA . regularExpressionToNFA . eParse

nfaOnscreen = previewDot . NGraphviz.toGraphviz . NFA.removeUnreachableStates
dfaOnscreen = previewDot . DGraphviz.toGraphviz

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
    let dict = buildDictionary $ sort inp
    return $ all (DMatcher.matches (mapDFAToDFA dict)) inp

prop_notAccepts = do
    inp <- genInpList
    let dict = buildDictionary $ sort inp
    notInp <- genInpList `suchThat` (Data.List.null . intersect inp)
    return $ not $ Data.List.any (DMatcher.matches $ mapDFAToDFA dict) notInp

prop_minStates = do
    inp <- resize 10 $ genInpList
    let dictNum = Data.Set.size $ DFA.states $ buildDictionary $ inp
    let minNum = Data.Set.size $ DFA.states $ minimize $ mapNFAToMapDFA $ trie $ inp
    return $ minNum == dictNum
