{-# LANGUAGE TupleSections #-}

-- | Functions and types modelling and working with DFAs.
-- Based on dragon book p.150
module Automata.DFA.MyDFA (
      MyDFA(..)
    , myDFAToDFA
    , fromKey
    , toKey
    , StateLabel
    , stateSize
    , states
    , buildDictionary
) where

import Control.Monad
import qualified Data.Map
import qualified Data.IntMap as Map
import qualified Data.Set
import qualified Data.IntSet as Set
import qualified Data.List as List
import Debug.Trace
import Prelude hiding (drop, length, init, tail, scanl, reverse, last, map, null, zip)
import Data.ByteString.Char8
import qualified Automata.DFA.Datatype as DFA
import Data.Maybe
import Data.Bits
import Data.Char

type StateLabel = Int

data MyDFA = MyDFA { transitionMap :: Map.IntMap StateLabel
                                  , invertMap :: Map.IntMap [StateLabel]
                                  , startKey :: StateLabel
                                  , final :: StateLabel
                                  , acceptKeys :: Set.IntSet
                                  } deriving (Show)

myDFAToDFA (MyDFA t _ s _ a) = DFA.DFA (transitionMapToFunction t) s (`Set.member` a)
    where transitionMapToFunction m s c = Map.lookup (toKey (s,c)) m

-- | Set of states in a DFA.
states (MyDFA m _ s _ _) = Set.unions [keyStates, valueStates, Set.singleton s]
    where keyStates = Set.map (fst . fromKey) (Map.keysSet m)
          valueStates = Set.fromList (Map.elems m)

stateSize (MyDFA m _ s _ _) = 1 + (List.length $ Map.keys m)

toKey :: (StateLabel, Char) -> Map.Key
toKey (s, c) = s `shiftL` 8 + ord c

fromKey :: Map.Key -> (StateLabel, Char)
fromKey k = (k `shiftR` 8, chr $ k `mod` 2^8)

commonPrefix xs ys | null xs || null ys = empty
                   | otherwise = let (x, xs') = fromJust $ uncons xs
                                     (y, ys') = fromJust $ uncons ys in
                                        if x==y then cons x (commonPrefix xs' ys') else empty


fst3 (x,_,_) = x

alphabet = ['a'..'z']

emptyDFA = MyDFA Map.empty Map.empty 0 0 Set.empty

trans dfa n e = toKey (n,e) `Map.lookup` (transitionMap dfa)
successors s dfa = List.map (trans dfa s) alphabet

isFinal s dfa = s `Set.member` acceptKeys dfa

checkEquiv :: MyDFA -> StateLabel -> StateLabel -> Bool
checkEquiv dfa s1 s2 = (s1 `isFinal` dfa == s2 `isFinal` dfa) && (s1 `successors` dfa == s2 `successors` dfa)

transStar :: MyDFA -> ByteString -> StateLabel
transStar dfa pref = foldl' (\n e -> fromJust $ trans dfa n e) (startKey dfa) pref

--list of states in dfa starting from state by traversing str
statesOnPath :: MyDFA -> StateLabel -> ByteString -> [StateLabel]
statesOnPath dfa state str = List.scanl (\n e -> fromJust $ trans dfa n e) state (unpack str)

predecessors :: MyDFA -> Char -> StateLabel -> [StateLabel]
predecessors dfa c state = (invertMap dfa) Map.! toKey (state, c) --List.map fst $ Map.keys $ Map.filter (==state) $ transitionMap dfa

findEquiv :: MyDFA -> StateLabel -> Char -> StateLabel -> Maybe StateLabel
findEquiv dfa state c goingTo = findEquivFrom dfa state $ predecessors dfa c goingTo

findEquivFrom :: MyDFA -> StateLabel -> [StateLabel] -> Maybe StateLabel
findEquivFrom dfa state l = listToMaybe $ List.filter (/= state) $ List.filter (checkEquiv dfa state) l

deleteState :: StateLabel -> MyDFA -> MyDFA 
deleteState state dfa = dfa {transitionMap = List.foldl' (\t c -> Map.delete (toKey (state,c)) t) (transitionMap dfa) (List.map snd succTrans), invertMap = delFromInvert}
    where succs = state `successors` dfa
          succTrans = catMaybes $ List.zipWith (\c s -> fmap (,c) s) alphabet succs
          delFromInvert = List.foldl' (\t sc -> Map.adjust (List.delete state) (toKey sc) t) (invertMap dfa) succTrans

insertTransition :: (StateLabel, Char, StateLabel) -> MyDFA -> MyDFA 
insertTransition (s1,x,s2) dfa = dfa {transitionMap = Map.insert (toKey (s1,x)) s2 (transitionMap dfa)
                                        , invertMap = Map.insertWith (++) (toKey (s2,x)) [s1] (invertMap dfa)}

buildDictionary :: [ByteString] -> MyDFA 
buildDictionary l = fst3 $ List.foldl' step (emptyDFA, empty, 1) l where
    step :: (MyDFA , ByteString, Int) -> ByteString -> (MyDFA , ByteString, Int)
    step (dfa, lastWord, newIndex) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex)
        | otherwise = (insertNewStates {final = newFinal}, newWord, newIndex + length newSuffix) where

        minimized = {-# SCC "minimized" #-} fst $ List.foldl' walkBack (dfa, Just $ final dfa) $ List.reverse $ candidatesTransitions

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
        newFinal = if null lastSuffix && final dfa == branchPoint then newIndex + length newSuffix - 1 else final dfa

        walkBack :: (MyDFA , Maybe StateLabel) -> (StateLabel, Char, StateLabel) -> (MyDFA , Maybe StateLabel)
        walkBack (dfa, Nothing) _ = (dfa, Nothing)
        walkBack (dfa, Just cur_st) (s1, x, s2)
            | cur_st == s2 = (dfa, Nothing)
            | otherwise = (newDfa, newCur) where
            newDfa = redirectTransition {acceptKeys = Set.delete s2 (acceptKeys dfa)}
            redirectTransition = insertTransition (s1,x,cur_st) $ deleteState s2 dfa
            newCur = findEquiv newDfa s1 x cur_st
