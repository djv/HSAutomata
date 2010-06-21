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
import qualified Data.List as List
import Debug.Trace
import Prelude hiding (drop, length, init, tail, scanl, reverse, last, map, null, zip)
import Data.ByteString.Char8

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
    = let setsToLabels = Map.fromList $ List.zip (Set.elems (states dfa)) [1..]
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

commonPrefix xs ys | null xs || null ys = empty
                   | otherwise = let (x, xs') = fromJust $ uncons xs
                                     (y, ys') = fromJust $ uncons ys in
                                        if x==y then cons x (commonPrefix xs' ys') else empty

fst3 (x,_,_,_) = x

trans t n e = (n,e) `Map.lookup` t
transAll t s = List.map (trans t s) ['a'..'c']

checkEquiv :: MapDFA StateLabel Char -> StateLabel -> StateLabel -> Bool
checkEquiv dfa s1 s2 = (transAll (transitionMap dfa) s1 == transAll (transitionMap dfa) s2) &&
    (s1 `Set.member` acceptKeys dfa == s2 `Set.member` acceptKeys dfa)

transStar :: MapDFA StateLabel Char -> ByteString -> StateLabel
transStar dfa pref = foldl' (\n e -> fromJust $ trans (transitionMap dfa) n e) (startKey dfa) pref

--list of states in dfa starting from state by traversing str
statesOnPath dfa state str = scanl (\n e -> fromJust $ trans (transitionMap dfa) n e) state str

predecessors dfa state = Map.filter (==state) $ transitionMap dfa

findEquiv dfa state goingTo = fmap (fst . fst) $ listToMaybe $ Map.toList $ Map.filterWithKey (\k a -> checkEquiv dfa state (fst k)) $ predecessors dfa goingTo

buildDictionary :: [ByteString] -> MapDFA StateLabel Char
buildDictionary l = fst3 $ List.foldl' step (MapDFA Map.empty 0 Set.empty, empty, 1, 0) l where
    step :: (MapDFA StateLabel Char, ByteString, Int, Int) -> ByteString -> (MapDFA StateLabel Char, ByteString, Int, Int)
    step (dfa, lastWord, newIndex, final) newWord
        | lastWord == newWord = (dfa, lastWord, newIndex, final)
        | otherwise = (MapDFA insertNewStates 0 insertNewFinal, newWord, newIndex + length newSuffix, newFinal) where

        minimized = fst $ List.foldl' walkBack (dfa, Just final) $ List.reverse $ candidatesTransitions

        --transitions on the lastSuffix path
        candidatesTransitions = List.zip3 (List.init candidates) (unpack lastSuffix) (List.tail candidates)

        --new state labels for the newSuffix path
        newStates = [newIndex .. newIndex + length newSuffix - 1]
        
        --insert newStates in minimized
        insertNewStates = List.foldl' (\dfa (s1, x, s2) -> Map.insert (s1,x) s2 dfa) (transitionMap minimized) $ List.zip3 (branchPoint:List.init newStates) (unpack newSuffix) newStates
        --mark last of newStates as final
        insertNewFinal = Set.insert (List.last newStates) (acceptKeys minimized)

        --change final if it is on the new path
        newFinal = if null lastSuffix then newIndex + length newSuffix - 1 else final

        pref = commonPrefix lastWord newWord
        lastSuffix = drop (length pref) lastWord
        newSuffix = drop (length pref) newWord

        branchPoint = transStar dfa pref

        --states on the path on lastSuffix which could be minimized
        candidates = statesOnPath dfa branchPoint lastSuffix

        walkBack :: (MapDFA StateLabel Char, Maybe StateLabel) -> (StateLabel, Char, StateLabel) -> (MapDFA StateLabel Char, Maybe StateLabel)
        walkBack (dfa, Nothing) _ = (dfa, Nothing)
        walkBack (dfa@(MapDFA t s a), Just cur_st) (s1, x, s2)
            | cur_st == s2 = (dfa, Nothing)
            | otherwise = (newDfa, newCur) where
            newDfa = dfa {transitionMap = redirectTransition, acceptKeys = Set.delete s2 a}
            redirectTransition = Map.insert (s1,x) cur_st $ deleteState s2 t
            newCur = findEquiv newDfa cur_st s1
            deleteState state trans = List.foldl' (\t c -> Map.delete (state,c) t) trans ['a'..'c']
