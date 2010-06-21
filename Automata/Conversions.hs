-- | Conversion between different Automata.
module Automata.Conversions (
      mapDFAToMapNFA
    , mapNFAToMapDFA
    , regularExpressionToNFA
    , mapNFAToComplexMapDFA -- remove me
    ) where

import Control.Arrow (second)

import qualified Data.List as List
import qualified Data.Map as Map    
import qualified Data.IntMap as IMap    
import qualified Data.Set as Set    

import Maybe

import qualified Automata.DFA.Datatype as DFA
import qualified Automata.NFA.Datatype as NFA
import Automata.NFA.Matcher
import Automata.RegularExpression.Datatype
import Automata.Utils (fix)

{- 
BugLog.

When working with an NFA, it's easy to make pseudo-type errors on whether or 
not a set of states is epsilon-closured.

Good and useful:
1. Start with an epsilon-closure of a set of states. 
   The NFA could be in any of these states.
2. Do something with all the non-epsilon moves 
   (the epsilon moves are accounted for already in the closure)
3. Take the epsilon-closure of the results. 


Not so good:
1. Start with a set of states
2. Gather all the non-epsilon and epsilon moves.
3. You're probably fucked. The epsilon moves of the current state are 
   with the current state. The non-epsilon moves are affiliated with
   future states. You should have a really good reason for mixing the two!

-}

mapDFAToMapNFA (DFA.MapDFA t i s a) 
    = NFA.MapNFA { NFA.transitionMap = Map.map Set.singleton $ Map.mapKeys (second Just) t
                 , NFA.startKey = s
                 , NFA.acceptKeys = a
                 }           

-- dragon book p.153                                           

mapNFAToMapDFA nfa = DFA.numberDFA $ mapNFAToComplexMapDFA nfa

               
{-
while there is an unmarked state T in dstates
    mark T
    for each input 'a'
        let U = epsilonClosure $ move T 'a'
            add U to dstates if not there already
            add a transition on 'a' from T to U
-}

type DState stateLabel = Set.Set (Bool, Set.Set stateLabel)

-- Get an unmarked state from the set of states.
-- Don't call on an empty set, will throw exception.
getUnmarkedState :: DState stateLabel -> Maybe (Set.Set stateLabel)
getUnmarkedState s = let (marked, stateSet) = Set.findMin s
                     in (if marked then Nothing else Just stateSet)

-- Mark a given state, return the new set.
mark s stateSet = Set.insert (True, stateSet) (Set.delete (False, stateSet) s)

-- Add a state to the set of states, unmarked.
addUnlessPresent s stateSet = case ((False, stateSet) `Set.member` s, (True, stateSet) `Set.member` s) of
                                       (False, False) -> Set.insert (False, stateSet) s
                                       _              -> s

transition t u a = Map.singleton (t, a) u
                                
initStateSet nfa = NFA.epsilonClosure (NFA.mapNFAToNFA nfa) (Set.singleton (NFA.startKey nfa))

acceptingStateSets nfa
    = Set.filter (\stateSet -> not $ Set.null $ stateSet `Set.intersection` NFA.acceptKeys nfa)

-- Map an NFA to a DFA in which state labels are Sets of the original NFA state labels.
mapNFAToComplexMapDFA nfa 
    = let (_dstates, transitionMap, accepting) = nfaToDFAHelper nfa
      in DFA.MapDFA { DFA.transitionMap = transitionMap
                    , DFA.invertMap = IMap.empty
                    , DFA.startKey = initStateSet nfa
                    , DFA.acceptKeys = accepting
                    }

-- Core implementation of the algorithm.      
-- TODO rename, reorganize, simplify, etc.      
nfaToDFAHelper' nfa (dstate, transitions, accepting)
  = let pureNFA = NFA.mapNFAToNFA nfa
    in       
      case getUnmarkedState dstate of -- While there is an unmarked state in dstates
          Nothing -> (dstate, transitions, accepting)--(transitions, accepting)
          Just t -> let dstate' = mark dstate t -- mark it
                        statesAndTransitions = Set.map (\a ->  -- for each input 'a'
                                                           let u = NFA.epsilonClosure pureNFA $ NFA.next pureNFA t a --let U = epsilonClosure $ move T 'a'
                                                           in (u, transition t u a))                                   
                                                       (NFA.relevantInputsFrom t nfa)
                        states = Set.map fst statesAndTransitions
                        transitions' = Set.elems $ Set.map snd statesAndTransitions
                        dstate'' = foldl addUnlessPresent dstate' (Set.elems states) -- add the Us to dstates if not there already
                        accepting' = accepting `Set.union` acceptingStateSets nfa states
                        transitions'' = transitions `Map.union` Map.unions transitions' -- add transition on 'a' from T to U
                    in (dstate'', transitions'', accepting')
                              
nfaToDFAHelper nfa 
    = fix (nfaToDFAHelper' nfa) 
          (addUnlessPresent Set.empty (initStateSet nfa),
           Map.empty,                                    
           (acceptingStateSets nfa) $ Set.singleton (initStateSet nfa))

regularExpressionToNFA :: Ord a => RegularExpression a -> NFA.MapNFA a
regularExpressionToNFA (Empty)             = NFA.empty
regularExpressionToNFA (Singleton a)       = NFA.singleton a
regularExpressionToNFA (Kleene r)          = NFA.kleene (regularExpressionToNFA r)
regularExpressionToNFA (Catenation r1 r2)  = NFA.catenate (regularExpressionToNFA r1) (regularExpressionToNFA r2)
regularExpressionToNFA (Alternation r1 r2) = NFA.alternate (regularExpressionToNFA r1) (regularExpressionToNFA r2)