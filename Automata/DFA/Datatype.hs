-- | Functions and types modelling and working with DFAs.
-- Based on dragon book p.150
module Automata.DFA.Datatype (
      DFA(..)
    , MapDFA(..)
    , StateLabel
    , mapDFAToDFA
    , states
    , numberDFA
    , relevantInputsFrom
    , mapStateLabels
    , isFrom
    , isTo
    , DTransducer (..)
    , compose
    , buildDictionary
) where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Debug.Trace

import Maybe

type StateLabel = Int

-- | Theoretical representation of DFAs. 
-- DFA type parameter 'a' is the type of alphabet
-- Set of states in the DFA is the transitive closure of (transition start)
data DFA stateLabel a = DFA { transition :: stateLabel -> a -> Maybe stateLabel
                            , start :: stateLabel
                            , accepting :: stateLabel -> Bool
                            }

-- | Pragmatic datatype for DFAs, suitable for manipulation.
-- Unfortunately, its use of Set and Map causes all sorts of Ord constraints
-- in functions that manipulate them.
data MapDFA stateLabel a = MapDFA { transitionMap :: Map.Map (stateLabel, a) stateLabel
                                  , startKey :: stateLabel
                                  , acceptKeys :: Set.Set stateLabel
                                  } deriving (Show)
-- | Untested basic datatype for transducers.
data DTransducer state a b =
    {- forall state. -} DTransducer (state -> a -> Maybe (state, b)) state

-- | Lift a pragmatic DFA to look like a theoretical representation of DFAs
mapDFAToDFA (MapDFA t s a) = DFA (transitionMapToFunction t) s (`Set.member` a)
    where transitionMapToFunction m = curry (`Map.lookup` m)

-- | Set of states in a DFA.
states (MapDFA m s _)= Set.unions [keyStates, valueStates, Set.singleton s]
    where keyStates = Set.map fst (Map.keysSet m)
          valueStates = Set.fromList (Map.elems m)

-- | compatibleWRT: make a DFA `compatible with respect to` another DFA.
-- Relabel the states of a MapDFA such that there is no overlap with the independent states in an 
-- unrelated MapDFA.
compatibleWRT dfa refDFA = mapStateLabels (+ (1 + Set.findMax (states refDFA))) dfa

numberDFA dfa 
    = let setsToLabels = Map.fromList $ zip (Set.elems (states dfa)) [1..]
          number = (setsToLabels Map.!)
      in  mapStateLabels number dfa

relevantInputsFrom stateSet (MapDFA m _ _) = 
  let keys = Map.keysSet m
      fromAnyOfS = Set.filter (\(label, ma) -> label `Set.member` stateSet) keys
  in Set.map snd fromAnyOfS

-- mapkeys and mapKeysMonotonic don't have compatible types we can't 
-- parameterize over them. Annoying.
mapStateLabels fn (MapDFA t s a)
    = MapDFA { transitionMap = Map.map fn (Map.mapKeys fnKeys t)
             , startKey = fn s
             , acceptKeys = Set.map fn a
             }
        where fnKeys (keyStateLabel, a) = (fn keyStateLabel, a)

-- | Predicate for filtering a DFA transitionMap for transitions from a given 
-- state, suitable for use with Map.filterWithKey.
isFrom :: Eq stateLabel => stateLabel -> (stateLabel, a) -> stateLabel -> Bool
isFrom from (candidate, _) _ = from == candidate

-- | Predicate for filtering a DFA transitionMap for transitions to a given
-- state, suitable for use with Map.filterWithKey.
isTo :: Eq stateLabel => stateLabel -> (stateLabel, a) -> stateLabel -> Bool
isTo to (_, _) candidate = to == candidate

compose :: DTransducer ts a [b] -> DFA ds b -> DFA (ts,ds) a
compose (DTransducer ttrans tstart) dfa =
    let
        trans (tstate,dstate) input =
            do (tstate', b) <- ttrans tstate input
               dstate' <- foldM (transition dfa) dstate b
               return (tstate', dstate')
    in
      DFA { transition = trans
          , start = (tstart, start dfa)
          , accepting = \(_,s) -> accepting dfa s
          }

commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (x:xs) (y:ys) = if x==y then x:commonPrefix xs ys else []

fst3 (x,_,_,_) = x

trans t n e = (n,e) `Map.lookup` t
transAll t s = map (trans t s) ['a'..'c']

buildDictionary :: [String] -> MapDFA StateLabel Char
buildDictionary l = fst3 $ foldl' step (MapDFA Map.empty 0 Set.empty, "", 1, 0) l where
    step :: (MapDFA StateLabel Char, String, Int, Int) -> String -> (MapDFA StateLabel Char, String, Int, Int)
    step (dfa, lastWord, newIndex, final) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex, final)
        | otherwise = {-# SCC "step" #-} (minimized {transitionMap = insertNewStates, acceptKeys = Set.insert (last newStates) (acceptKeys dfa)}, newWord, newIndex + length newSuffix, newFinal) where
        minimized = {-# SCC "minimized" #-} fst $ foldl' walkBack (dfa, Just final) $ reverse $ candidatesTransitions
        candidatesTransitions = zip3 (init candidates) lastSuffix (tail candidates)
        newStates = [newIndex .. newIndex + length newSuffix - 1]
        insertNewStates = {-# SCC "insertNewStates" #-} foldl' (\dfa (s1, x, s2) -> Map.insert (s1,x) s2 dfa) (transitionMap minimized) $ zip3 (branchPoint:init newStates) newSuffix newStates
        newFinal = if List.null lastSuffix then newIndex + length newSuffix - 1 else final

        pref = commonPrefix lastWord newWord

        branchPoint :: StateLabel
        branchPoint = {-# SCC "branchPoint" #-} foldl' (\n e -> fromJust $ trans (transitionMap dfa) n e) (startKey dfa) pref

        lastSuffix = fromJust $ stripPrefix pref lastWord
        newSuffix = fromJust $ stripPrefix pref newWord

        candidates :: [StateLabel]
        candidates = {-# SCC "candidates" #-} scanl (\n e -> fromJust $ trans (transitionMap dfa) n e) branchPoint lastSuffix

        checkEquiv :: Map.Map (StateLabel, Char) StateLabel -> StateLabel -> StateLabel -> Bool
        checkEquiv t s1 s2 = {-# SCC "checkEquiv" #-} (transAll t s1 == transAll t s2) &&
            (s1 `Set.member` acceptKeys dfa == s2 `Set.member` acceptKeys dfa) 

        walkBack :: (MapDFA StateLabel Char, Maybe StateLabel) -> (StateLabel, Char, StateLabel) -> (MapDFA StateLabel Char, Maybe StateLabel)
        walkBack (dfa, Nothing) _ = (dfa, Nothing)
        walkBack (dfa@(MapDFA t s a), Just cur_st) (s1, x, s2)
            | cur_st == s2 = (dfa, Nothing)
            | otherwise = {-# SCC "walkBack" #-} (dfa {transitionMap = redirectTransition, acceptKeys = traceShow (s2,a) $ Set.delete s2 a}, newCur) where
            redirectTransition = Map.insert (s1,x) cur_st $ deleteState s2 t
            newCur = {-# SCC "newCur" #-} fmap (fst . fst) $ listToMaybe $ Map.toList $ Map.filterWithKey (\k a -> checkEquiv redirectTransition s1 (fst k)) $ {-# SCC "filter_isTo" #-} Map.filterWithKey (isTo cur_st) t
            deleteState state trans = {-# SCC "deleteState" #-} foldl' (\t c -> Map.delete (state,c) t) trans ['a'..'c']
