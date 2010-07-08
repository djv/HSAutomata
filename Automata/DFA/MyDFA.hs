{-# LANGUAGE TupleSections, ExistentialQuantification, BangPatterns #-}

-- | Functions and types modelling and working with DFAs.
-- Based on dragon book p.150
module Automata.DFA.MyDFA (
      MyDFA(..)
    , Regex(..)
    , matchRegex
    , myDFAToDFA
    , determinize
    , fromKey
    , toKey
    , State
    , stateSize
    , stateSizeReal
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
import Data.List
import Debug.Trace
import Prelude hiding (drop, length, init, tail, scanl, reverse, last, map, null, zip, lines, readFile, take)
import qualified Data.ByteString.Char8 as B
import qualified Automata.DFA.Datatype as DFA
import Data.Maybe
import Data.Bits
import Data.Char
import qualified Data.Queue.Class as Q
import qualified Data.Queue.Stack as QInst

type State = Int
type StateSet = Set.IntSet
type Transitions = Map.IntMap
type SetTransition = (StateSet, Char, StateSet)
type FP = Int
type Queue = QInst.Stack (State, (Char, StateSet))

data MyDFA = MyDFA { transitionMap :: Transitions State
                    , invertMap :: Transitions StateSet
                    , startKey :: State
                    , final :: State
                    , transFP :: Map.IntMap StateSet
                    , validTrans :: Map.IntMap [Char]
                    , acceptKeys :: StateSet
                    } deriving (Show)

emptyDFA = MyDFA Map.empty Map.empty 0 0 Map.empty Map.empty Set.empty

proccessQueue :: (Q.IQueue q) => (q,a) -> ((q,a) -> (q,a)) -> a
proccessQueue (q,val) f | Q.null q = val
                        | otherwise = proccessQueue (f (q,val)) f

type Visited = (M.Map StateSet State, Map.IntMap State)

lookupVisited :: StateSet -> Visited -> Maybe State
lookupVisited set (multi, single) | Set.size set == 1 = Map.lookup (Set.findMin set) single
                                  | otherwise = M.lookup set multi

insertVisited :: StateSet -> State -> Visited -> Visited
insertVisited set index (multi, single) | Set.size set == 1 = (multi, Map.insert (Set.findMin set) index single)
                                        | otherwise = (M.insert set index multi, single)

determinize :: MyDFA -> MyDFA
determinize dfa = fst3 $ determinize' (invertMap dfa) (acceptKeys dfa) (startKey dfa)

determinize' :: Transitions StateSet -> StateSet -> State -> (MyDFA, Visited, State)
determinize' transMap start final = proccessQueue ((Q.fromList $ zip (repeat 0) (transSetAll transMap start)) :: Queue, (emptyDFA, (M.singleton start 0, Map.empty), 1)) step where 
    step :: (Queue, (MyDFA, Visited, State)) -> (Queue, (MyDFA, Visited, State))
    step (!q, (!det_dfa, !visited, !newIndex)) = case lookupVisited to visited of
                                                       Nothing -> (Q.insertAll (zip (repeat newIndex) (transSetAll transMap to)) q', (insertTransition (det_dfa {acceptKeys = newAccept}) (from, char, newIndex), insertVisited to newIndex visited, newIndex + 1))
                                                       Just toLabel -> (q', (insertTransition det_dfa (from, char, toLabel), visited, newIndex))
                                                       where newAccept = if final `Set.member` to then Set.insert newIndex (acceptKeys det_dfa) else acceptKeys det_dfa
                                                             ((from, (char, to)), q') = fromJust $ Q.extract q

transSetAll :: Transitions StateSet -> StateSet -> [(Char, StateSet)]
transSetAll t s = filter (not . Set.null . snd) $ zip alphabet (map (transSet t s) alphabet)

transSet :: Transitions StateSet -> StateSet -> Char -> StateSet
transSet t s c = Set.unions $ catMaybes $ map (\k -> Map.lookup (toKey (k,c)) t) $ Set.toList s

myDFAToDFA dfa = DFA.DFA (transitionMapToFunction $ transitionMap dfa) (startKey dfa) (`Set.member` acceptKeys dfa)
    where transitionMapToFunction m s c = Map.lookup (toKey (s,c)) m

-- | Set of states in a DFA.
states dfa = Set.unions [keyStates, valueStates, Set.singleton $ startKey dfa]
    where keyStates = Set.map (fst . fromKey) (Map.keysSet $ transitionMap dfa)
          valueStates = Set.fromList (Map.elems $ transitionMap dfa)

stateSizeReal dfa = Set.size $ states dfa
stateSize dfa = 1 + (length $ Map.keys $ transitionMap dfa)

toKey :: (State, Char) -> Map.Key
toKey (s, c) = s `shiftL` 8 + ord c

fromKey :: Map.Key -> (State, Char)
fromKey k = (k `shiftR` 8, chr $ k `mod` 2^8)

commonPrefix xs ys | B.null xs || B.null ys = B.empty
                   | otherwise = let (x, xs') = fromJust $ B.uncons xs
                                     (y, ys') = fromJust $ B.uncons ys in
                                        if x==y then B.cons x (commonPrefix xs' ys') else B.empty

fst3 (x,_,_) = x

alphabet = ['a'..'z']

tr dfa st c =  toKey (st,c) `Map.lookup` (transitionMap dfa)

trans dfa n e = transitionMap dfa Map.! toKey (n,e)
successors s dfa = map (trans dfa s) $ findValidTrans dfa s

findValidTrans :: MyDFA -> State -> [Char]
findValidTrans dfa s = Map.findWithDefault "" s (validTrans dfa)

str2fp :: [Char] -> FP
str2fp str = foldl' (\n c -> n `xor` bit c) 0 $ map char2bit str

findFP dfa state = str2fp $ findValidTrans dfa state

char2bit :: Char -> Int
char2bit c = ord c - 97

changeFP tfp s newFP oldFP = Map.insertWith Set.union newFP (Set.singleton s) $ deleteFP tfp s oldFP
deleteFP tfp s fp = Map.adjust (Set.delete s) fp tfp

isFinal :: State -> MyDFA -> Bool
isFinal s dfa = s `Set.member` acceptKeys dfa

checkEquiv :: MyDFA -> Bool -> [State] -> State -> Bool
checkEquiv dfa finality succs s2 = (finality == s2 `isFinal` dfa) && (succs == s2 `successors` dfa)

transStar :: MyDFA -> B.ByteString -> State
transStar dfa word = B.foldl' (\n e -> trans dfa n e) (startKey dfa) word

--list of states in dfa starting from state by traversing str
statesOnPath :: MyDFA -> State -> B.ByteString -> [State]
statesOnPath dfa state str = scanl (\n e -> trans dfa n e) state (B.unpack str)

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
          succTrans = zipWith (flip (,)) trans succs
          updateTransition = foldl' (\t c -> Map.delete (toKey (state,c)) t) (transitionMap dfa) (map snd succTrans)
          delFromInvert = foldl' (\t succPair -> Map.adjust (Set.delete state) (toKey succPair) t) (invertMap dfa) succTrans
          delFromValidTrans = Map.delete state (validTrans dfa)
          delFromTransFP = deleteFP (transFP dfa) state (str2fp trans)

insertTransition :: MyDFA -> (State, Char, State) -> MyDFA 
insertTransition !dfa (!s1,!x,!s2) = dfa {transitionMap = Map.insert (toKey (s1,x)) s2 $! (transitionMap dfa)
                                        , invertMap = Map.insertWith (Set.union) (toKey (s2,x)) (Set.singleton s1) $! (invertMap dfa)}

insertForwardTransition :: MyDFA -> (State, Char, State) -> MyDFA 
insertForwardTransition !dfa (!s1,!x,!s2) = dfa {transitionMap = Map.insert (toKey (s1,x)) s2 $! (transitionMap dfa)}

insertBackwardTransition :: MyDFA -> (State, Char, State) -> MyDFA 
insertBackwardTransition !dfa (!s1,!x,!s2) = dfa {invertMap = Map.insertWith (Set.union) (toKey (s2,x)) (Set.singleton s1) $! (invertMap dfa)}

redirectTransition dfa s1 x s2 to = insertTransition (deleteState s2 dfa) (s1,x,to)

splitPrefix w1 w2 = (pref, w1suf, w2suf) where 
    pref = commonPrefix w1 w2
    w1suf = B.drop (B.length pref) w1
    w2suf = B.drop (B.length pref) w2

buildDictionary :: [B.ByteString] -> MyDFA 
buildDictionary l = fst3 $ foldl' step (emptyDFA, B.empty, 1) l where
    step :: (MyDFA , B.ByteString, Int) -> B.ByteString -> (MyDFA , B.ByteString, Int)
    step (dfa, lastWord, newIndex) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex)
        | otherwise = (insertNewStates {final = newFinal}, newWord, newIndex + B.length newSuffix) where

        minimized = fst $ foldl' walkBack (dfa, Just $ final dfa) $ reverse $ candidatesTransitions

        (pref, lastSuffix, newSuffix) = splitPrefix lastWord newWord
        branchPoint = transStar dfa pref

        --states on the path on lastSuffix which could be minimized
        candidates = statesOnPath dfa branchPoint lastSuffix
        --transitions on the lastSuffix path
        candidatesTransitions = zip3 (init candidates) (B.unpack lastSuffix) (tail candidates)
        --new state labels for the newSuffix path
        newStates = [newIndex .. newIndex + B.length newSuffix - 1]
        --insert newStates in minimized
        insertNewStates = (foldl' insertTransition minimized $ zip3 (branchPoint:init newStates) (B.unpack newSuffix) newStates) {acceptKeys = insertNewFinal, validTrans = updateValidTrans, transFP = updateTransFP} where
            --mark last of newStates as final
            insertNewFinal = Set.insert (last newStates) (acceptKeys minimized)
            updateValidTrans = foldl' (\v (s,c) -> Map.insertWith (++) s [c] v) (validTrans dfa) transitions
            transitions :: [(State, Char)]
            transitions = zip (branchPoint:init newStates) (B.unpack newSuffix)
            updateTransFP = foldl' (\tfp (state,c) -> let oldFP = findFP dfa state in changeFP tfp state (oldFP `xor` bit (char2bit c)) oldFP) (transFP dfa) transitions

        --change final if it is on the new path
        newFinal = if B.null lastSuffix && final dfa == branchPoint then newIndex + B.length newSuffix - 1 else final dfa

        walkBack :: (MyDFA , Maybe State) -> (State, Char, State) -> (MyDFA , Maybe State)
        walkBack (dfa, Nothing) _ = (dfa, Nothing)
        walkBack (dfa, Just cur_st) (s1, x, s2)
            | cur_st == s2 = (dfa, Nothing)
            | otherwise = (newDfa, newCur) where
            newDfa = redirect {acceptKeys = Set.delete s2 (acceptKeys dfa)}
            redirect = redirectTransition dfa s1 x s2 cur_st
            newCur = findEquiv newDfa s1 x cur_st

buildTrie :: [B.ByteString] -> MyDFA 
buildTrie l = fst3 $ foldl' step (emptyDFA, B.empty, 1) $ sort $ map B.reverse l where
    step :: (MyDFA , B.ByteString, Int) -> B.ByteString -> (MyDFA , B.ByteString, Int)
    step (!dfa, !lastWord, !newIndex) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex)
        | otherwise = (insertNewStates, newWord, newIndex + B.length newSuffix) where
        
        (pref, lastSuffix, newSuffix) = splitPrefix lastWord newWord
        branchPoint = transStar dfa pref

        --new state labels for the newSuffix path
        newStates = [newIndex .. newIndex + B.length newSuffix - 1]
        --insert newStates
        insertNewStates = (foldl' insertTransition dfa $ zip3 (branchPoint:init newStates) (B.unpack newSuffix) newStates) {acceptKeys = insertNewFinal} where
            --mark last of newStates as final
            insertNewFinal = Set.insert (last newStates) (acceptKeys dfa)

data Regex a = Null | Empty | Symbol a | Sum [Regex a] | Concat [Regex a] | Star (Regex a)
    deriving (Eq, Ord, Show)

matchRegex :: MyDFA -> Regex Char -> [String]
matchRegex dfa regex = map snd $ filter ((`isFinal` dfa) . fst) $ match' dfa (startKey dfa) "" regex where
    match' :: MyDFA -> State -> String -> Regex Char -> [(State, String)]
    match' _ _ _ Null = []
    match' dfa state str Empty = [(state, str)]
    match' dfa state str (Symbol c) = case tr dfa state c of
                                          Nothing -> []
                                          Just st2 -> match' dfa st2 (str ++ [c]) Empty
    match' dfa state str (Sum rs) = concatMap (match' dfa state str) rs
    match' dfa state str (Concat []) = match' dfa state str Empty
    match' dfa state str (Concat (r:rs)) = concatMap (\(state', str') -> match' dfa state' str' (Concat rs)) res where
        res = match' dfa state str r
    match' dfa state str (Star r) = match' dfa state str (Sum [Empty, Concat [r, Star r]])
