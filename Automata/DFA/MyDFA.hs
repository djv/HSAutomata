{-# LANGUAGE TupleSections #-}

-- | Functions and types modelling and working with DFAs.
-- Based on dragon book p.150
module Automata.DFA.MyDFA (
      MyDFA(..)
    , myDFAToDFA
    , determinize
    , fromKey
    , toKey
    , State
    , stateSize
    , states
    , transSetAll
    , buildDictionary
    , buildTrie
) where

import Control.Monad
import Control.Arrow
import qualified Data.Map as M
import qualified Data.IntMap as Map
import qualified Data.Set as S
import qualified Data.IntSet as Set
import qualified Data.List as List
import Debug.Trace
import Prelude hiding (drop, length, init, tail, scanl, reverse, last, map, null, zip, lines, readFile, take)
import Data.ByteString.Char8
import qualified Automata.DFA.Datatype as DFA
import Data.Maybe
import Data.Bits
import Data.Char

type State = Int
type StateSet = Set.IntSet
type Transitions = Map.IntMap
type FP = Int

data MyDFA = MyDFA { transitionMap :: Transitions State
                    , invertMap :: Transitions StateSet
                    , startKey :: State
                    , final :: State
                    , transFP :: Map.IntMap StateSet
                    , validTrans :: Map.IntMap [Char]
                    , acceptKeys :: StateSet
                    } deriving (Show)

emptyDFA = MyDFA Map.empty Map.empty 0 0 Map.empty Map.empty Set.empty

--determinize :: MyDFA -> Transitions State
determinize dfa = List.foldl' step (emptyDFA, M.singleton start 0, 1) $ transSetAll (invertMap dfa) start where 
    start = acceptKeys dfa
    step (dfa, visited, newIndex) (from, char, to) = case M.lookup to visited of
                                                       Nothing -> (insertTransition (visited M.! from, char, newIndex) (dfa {acceptKeys = newAccept}), M.insert to newIndex visited, newIndex + 1)
                                                       Just toLabel -> (insertTransition (visited M.! from, char, toLabel) dfa, visited, newIndex)
                                                       where newAccept = if startKey dfa `Set.member` to then Set.insert newIndex (acceptKeys dfa) else acceptKeys dfa

transSetAll :: Transitions StateSet -> StateSet -> [(StateSet, Char, StateSet)]
transSetAll t s = List.filter (not . Set.null . (\(_,_,s)->s)) $ List.zip3 (List.repeat s) alphabet (List.map (transSet t s) alphabet)

transSet :: Transitions StateSet -> StateSet -> Char -> StateSet
transSet t s c = Set.unions $ catMaybes $ List.map (\k -> Map.lookup (toKey (k,c)) t) $ Set.toList s

myDFAToDFA dfa = DFA.DFA (transitionMapToFunction $ transitionMap dfa) (startKey dfa) (`Set.member` acceptKeys dfa)
    where transitionMapToFunction m s c = Map.lookup (toKey (s,c)) m

-- | Set of states in a DFA.
states dfa = Set.unions [keyStates, valueStates, Set.singleton $ startKey dfa]
    where keyStates = Set.map (fst . fromKey) (Map.keysSet $ transitionMap dfa)
          valueStates = Set.fromList (Map.elems $ transitionMap dfa)

stateSize dfa = 1 + (List.length $ Map.keys $ transitionMap dfa)

toKey :: (State, Char) -> Map.Key
toKey (s, c) = s `shiftL` 8 + ord c

fromKey :: Map.Key -> (State, Char)
fromKey k = (k `shiftR` 8, chr $ k `mod` 2^8)

commonPrefix xs ys | null xs || null ys = empty
                   | otherwise = let (x, xs') = fromJust $ uncons xs
                                     (y, ys') = fromJust $ uncons ys in
                                        if x==y then cons x (commonPrefix xs' ys') else empty

fst3 (x,_,_) = x

alphabet = ['a'..'z']

trans dfa n e = transitionMap dfa Map.! toKey (n,e) --toKey (n,e) `Map.lookup` (transitionMap dfa)
successors s dfa = List.map (trans dfa s) $ findValidTrans dfa s

findValidTrans :: MyDFA -> State -> [Char]
findValidTrans dfa s = Map.findWithDefault "" s (validTrans dfa)

str2fp :: [Char] -> FP
str2fp str = List.foldl' (\n c -> n `xor` bit c) 0 $ List.map char2bit str

findFP dfa state = str2fp $ findValidTrans dfa state

char2bit :: Char -> Int
char2bit c = ord c - 97

changeFP tfp s newFP oldFP = Map.insertWith Set.union newFP (Set.singleton s) $ deleteFP tfp s oldFP
deleteFP tfp s fp = Map.adjust (Set.delete s) fp tfp

isFinal :: State -> MyDFA -> Bool
isFinal s dfa = s `Set.member` acceptKeys dfa

checkEquiv :: MyDFA -> Bool -> [State] -> State -> Bool
checkEquiv dfa finality succs s2 = (finality == s2 `isFinal` dfa) && (succs == s2 `successors` dfa)

transStar :: MyDFA -> ByteString -> State
transStar dfa word = foldl' (\n e -> trans dfa n e) (startKey dfa) word

--list of states in dfa starting from state by traversing str
statesOnPath :: MyDFA -> State -> ByteString -> [State]
statesOnPath dfa state str = List.scanl (\n e -> trans dfa n e) state (unpack str)

predecessors :: MyDFA -> Char -> State -> StateSet
predecessors dfa c state = (invertMap dfa) Map.! toKey (state, c) --List.map fst $ Map.keys $ Map.filter (==state) $ transitionMap dfa

findEquiv :: MyDFA -> State -> Char -> State -> Maybe State
findEquiv dfa state c goingTo = findEquivFrom dfa state $ (preds `Set.intersection` sameFP) where
    preds = predecessors dfa c goingTo
    sameFP = Map.findWithDefault Set.empty (findFP dfa state) (transFP dfa)

findEquivFrom :: MyDFA -> State -> StateSet -> Maybe State
findEquivFrom dfa state l = listToMaybe $ Set.toList $ Set.filter (/= state) $ Set.filter (checkEquiv dfa (state `isFinal` dfa) (state `successors` dfa)) l

deleteState :: State -> MyDFA -> MyDFA 
deleteState state dfa = dfa {transitionMap = updateTransition, invertMap = delFromInvert, validTrans = delFromValidTrans, transFP = delFromTransFP}
    where succs = state `successors` dfa
          trans = findValidTrans dfa state
          succTrans = List.zipWith (flip (,)) trans succs
          updateTransition = List.foldl' (\t c -> Map.delete (toKey (state,c)) t) (transitionMap dfa) (List.map snd succTrans)
          delFromInvert = List.foldl' (\t succPair -> Map.adjust (Set.delete state) (toKey succPair) t) (invertMap dfa) succTrans
          delFromValidTrans = Map.delete state (validTrans dfa)
          delFromTransFP = deleteFP (transFP dfa) state (str2fp trans)

insertTransition :: (State, Char, State) -> MyDFA -> MyDFA 
insertTransition (s1,x,s2) dfa = dfa {transitionMap = Map.insert (toKey (s1,x)) s2 (transitionMap dfa)
                                        , invertMap = Map.insertWith (Set.union) (toKey (s2,x)) (Set.singleton s1) (invertMap dfa)}

redirectTransition dfa s1 x s2 to = insertTransition (s1,x,to) $ deleteState s2 dfa

splitPrefix w1 w2 = (pref, w1suf, w2suf) where 
    pref = commonPrefix w1 w2
    w1suf = drop (length pref) w1
    w2suf = drop (length pref) w2

buildDictionary :: [ByteString] -> MyDFA 
buildDictionary l = fst3 $ List.foldl' step (emptyDFA, empty, 1) l where
    step :: (MyDFA , ByteString, Int) -> ByteString -> (MyDFA , ByteString, Int)
    step (dfa, lastWord, newIndex) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex)
        | otherwise = (insertNewStates {final = newFinal}, newWord, newIndex + length newSuffix) where

        minimized = {-# SCC "minimized" #-} fst $ List.foldl' walkBack (dfa, Just $ final dfa) $ List.reverse $ candidatesTransitions

        (pref, lastSuffix, newSuffix) = splitPrefix lastWord newWord
        branchPoint = transStar dfa pref

        --states on the path on lastSuffix which could be minimized
        candidates = statesOnPath dfa branchPoint lastSuffix
        --transitions on the lastSuffix path
        candidatesTransitions = List.zip3 (List.init candidates) (unpack lastSuffix) (List.tail candidates)
        --new state labels for the newSuffix path
        newStates = [newIndex .. newIndex + length newSuffix - 1]
        --insert newStates in minimized
        insertNewStates = (List.foldl' (flip insertTransition) minimized $ List.zip3 (branchPoint:List.init newStates) (unpack newSuffix) newStates) {acceptKeys = insertNewFinal, validTrans = updateValidTrans, transFP = updateTransFP} where
            --mark last of newStates as final
            insertNewFinal = Set.insert (List.last newStates) (acceptKeys minimized)
            updateValidTrans = List.foldl' (\v (s,c) -> Map.insertWith (++) s [c] v) (validTrans dfa) transitions
            transitions :: [(State, Char)]
            transitions = List.zip (branchPoint:List.init newStates) (unpack newSuffix)
            updateTransFP = List.foldl' (\tfp (state,c) -> let oldFP = findFP dfa state in changeFP tfp state (oldFP `xor` bit (char2bit c)) oldFP) (transFP dfa) transitions

        --change final if it is on the new path
        newFinal = if null lastSuffix && final dfa == branchPoint then newIndex + length newSuffix - 1 else final dfa

        walkBack :: (MyDFA , Maybe State) -> (State, Char, State) -> (MyDFA , Maybe State)
        walkBack (dfa, Nothing) _ = (dfa, Nothing)
        walkBack (dfa, Just cur_st) (s1, x, s2)
            | cur_st == s2 = (dfa, Nothing)
            | otherwise = (newDfa, newCur) where
            newDfa = redirect {acceptKeys = Set.delete s2 (acceptKeys dfa)}
            redirect = redirectTransition dfa s1 x s2 cur_st
            newCur = findEquiv newDfa s1 x cur_st

buildTrie :: [ByteString] -> MyDFA 
buildTrie l = fst3 $ List.foldl' step (emptyDFA, empty, 1) l where
    step :: (MyDFA , ByteString, Int) -> ByteString -> (MyDFA , ByteString, Int)
    step (dfa, lastWord, newIndex) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex)
        | otherwise = (insertNewStates, newWord, newIndex + length newSuffix) where

        (pref, lastSuffix, newSuffix) = splitPrefix lastWord newWord
        branchPoint = {-# SCC "branchpoint" #-} transStar dfa pref

        --new state labels for the newSuffix path
        newStates = [newIndex .. newIndex + length newSuffix - 1]
        --insert newStates in minimized
        insertNewStates = (List.foldl' (flip insertTransition) dfa $ List.zip3 (branchPoint:List.init newStates) (unpack newSuffix) newStates) {acceptKeys = insertNewFinal, validTrans = updateValidTrans} where
            --mark last of newStates as final
            insertNewFinal = {-# SCC "insertNew:insertFinal" #-} Set.insert (List.last newStates) (acceptKeys dfa)
            updateValidTrans = {-# SCC "insertNew:updateValid" #-} List.foldl' (\v (s,c) -> Map.insertWith (++) s [c] v) (validTrans dfa) transitions
            transitions :: [(State, Char)]
            transitions = {-# SCC "insertNew:transitions" #-} List.zip (branchPoint:List.init newStates) (unpack newSuffix)
