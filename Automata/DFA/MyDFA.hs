{-# LANGUAGE TupleSections #-}

-- | Functions and types modelling and working with DFAs.
-- Based on dragon book p.150
module Automata.DFA.MyDFA (
      MyDFA(..)
    , myDFAToDFA
    , StateLabel
    , stateSize
    , states
    , buildDictionary
) where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Set
import qualified Data.IntSet as Set
import qualified Data.List as List
import Debug.Trace
import Prelude hiding (drop, length, init, tail, scanl, reverse, last, map, null, zip)
import Data.ByteString.Char8
import qualified Automata.DFA.Datatype as DFA
import Maybe

type StateLabel = Int

data MyDFA stateLabel a = MyDFA { transitionMap :: Map.Map (stateLabel, a) stateLabel
                                  , invertMap :: IMap.IntMap [StateLabel]
                                  , startKey :: stateLabel
                                  , acceptKeys :: Set.IntSet
                                  } deriving (Show)

myDFAToDFA (MyDFA t _ s a) = DFA.DFA (transitionMapToFunction t) s (`Set.member` a)
    where transitionMapToFunction m = curry (`Map.lookup` m)

-- | Set of states in a DFA.
states (MyDFA m _ s _) = Data.Set.unions [keyStates, valueStates, Data.Set.singleton s]
    where keyStates = Data.Set.map fst (Map.keysSet m)
          valueStates = Data.Set.fromList (Map.elems m)

stateSize (MyDFA m _ s _) = 1 + (List.length $ Map.keys m)

commonPrefix xs ys | null xs || null ys = empty
                   | otherwise = let (x, xs') = fromJust $ uncons xs
                                     (y, ys') = fromJust $ uncons ys in
                                        if x==y then cons x (commonPrefix xs' ys') else empty


fst3 (x,_,_,_) = x

alphabet = ['a'..'c']

emptyDFA = MyDFA Map.empty IMap.empty 0 Set.empty

trans dfa n e = (n,e) `Map.lookup` (transitionMap dfa)
successors s dfa = List.map (trans dfa s) alphabet

isFinal s dfa = s `Set.member` acceptKeys dfa

checkEquiv :: MyDFA StateLabel Char -> StateLabel -> StateLabel -> Bool
checkEquiv dfa s1 s2 = (s1 `isFinal` dfa == s2 `isFinal` dfa) && (s1 `successors` dfa == s2 `successors` dfa)

transStar :: MyDFA StateLabel Char -> ByteString -> StateLabel
transStar dfa pref = foldl' (\n e -> fromJust $ trans dfa n e) (startKey dfa) pref

--list of states in dfa starting from state by traversing str
statesOnPath :: MyDFA StateLabel Char -> StateLabel -> ByteString -> [StateLabel]
statesOnPath dfa state str = List.scanl (\n e -> fromJust $ trans dfa n e) state (unpack str)

predecessors :: MyDFA StateLabel Char -> StateLabel -> [StateLabel]
predecessors dfa state = (invertMap dfa) IMap.! state --List.map fst $ Map.keys $ Map.filter (==state) $ transitionMap dfa

findEquiv :: MyDFA StateLabel Char -> StateLabel -> StateLabel -> Maybe StateLabel
findEquiv dfa state goingTo = listToMaybe $ List.filter (/= state) $ List.filter (checkEquiv dfa state) $ predecessors dfa goingTo

deleteState :: StateLabel -> MyDFA StateLabel Char -> MyDFA StateLabel Char
deleteState state dfa = dfa {transitionMap = List.foldl' (\t c -> Map.delete (state,c) t) (transitionMap dfa) (List.map fst succTrans), invertMap = delFromInvert}
    where succs = state `successors` dfa
          succTrans = catMaybes $ List.zipWith (\c s -> fmap (c,) s) alphabet succs
          delFromInvert = List.foldl' (\t s2 -> IMap.adjust (List.delete state) s2 t) (invertMap dfa) (List.map snd succTrans)

insertTransition :: (StateLabel, Char, StateLabel) -> MyDFA StateLabel Char -> MyDFA StateLabel Char
insertTransition (s1,x,s2) dfa = dfa {transitionMap = Map.insert (s1,x) s2 (transitionMap dfa)
                                        , invertMap = IMap.insertWith (++) s2 [s1] (invertMap dfa)}

buildDictionary :: [ByteString] -> MyDFA StateLabel Char
buildDictionary l = fst3 $ List.foldl' step (emptyDFA, empty, 1, 0) l where
    step :: (MyDFA StateLabel Char, ByteString, Int, Int) -> ByteString -> (MyDFA StateLabel Char, ByteString, Int, Int)
    step (dfa, lastWord, newIndex, final) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex, final)
        | otherwise = (insertNewStates, newWord, newIndex + length newSuffix, newFinal) where

        minimized = fst $ List.foldl' walkBack (dfa, Just final) $ List.reverse $ candidatesTransitions

        pref = commonPrefix lastWord newWord
        lastSuffix = drop (length pref) lastWord
        newSuffix = drop (length pref) newWord

        branchPoint = transStar dfa pref

        --states on the path on lastSuffix which could be minimized
        candidates = statesOnPath dfa branchPoint lastSuffix
        --transitions on the lastSuffix path
        candidatesTransitions = List.zip3 (List.init candidates) (unpack lastSuffix) (List.tail candidates)
        --new state labels for the newSuffix path
        newStates = [newIndex .. newIndex + length newSuffix - 1]
        --insert newStates in minimized
        insertNewStates = (List.foldl' (flip insertTransition) minimized $ List.zip3 (branchPoint:List.init newStates) (unpack newSuffix) newStates) {acceptKeys = insertNewFinal} where
            --mark last of newStates as final
            insertNewFinal = Set.insert (List.last newStates) (acceptKeys minimized)

        --change final if it is on the new path
        newFinal = if null lastSuffix && final == branchPoint then newIndex + length newSuffix - 1 else final

        walkBack :: (MyDFA StateLabel Char, Maybe StateLabel) -> (StateLabel, Char, StateLabel) -> (MyDFA StateLabel Char, Maybe StateLabel)
        walkBack (dfa, Nothing) _ = (dfa, Nothing)
        walkBack (dfa, Just cur_st) (s1, x, s2)
            | cur_st == s2 = (dfa, Nothing)
            | otherwise = (newDfa, newCur) where
            newDfa = redirectTransition {acceptKeys = Set.delete s2 (acceptKeys dfa)}
            redirectTransition = insertTransition (s1,x,cur_st) $ deleteState s2 dfa
            newCur = findEquiv newDfa s1 cur_st
