{-# LANGUAGE TupleSections #-}

-- | Functions and types modelling and working with DFAs.
-- Based on dragon book p.150
module Automata.DFA.Datatype (
      DFA(..)
    , MapDFA(..)
    , StateLabel
    , mapDFAToDFA
    , numberDFA
    , relevantInputsFrom
    , mapStateLabels
    , states
    , isFrom
    , isTo
    , DTransducer (..)
    , compose
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
states (MapDFA m s _) = Set.unions [keyStates, valueStates, Set.singleton s]
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
