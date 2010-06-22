-- | Constructors and combinators for NFAs. 
-- Theoretical representation and pragmatic representation.
module Automata.NFA.Datatype (
      StateLabel
    , NFA (..)
    , MapNFA (..)
    , epsilonClosure
    , next
    , mapNFAToNFA
    , states
    , removeUnreachableStates
    , mapStateLabels
    , isFrom
    , relevantInputsFrom
    , empty
    , singleton
    , catenate
    , alternate
    , optional
    , loop
    , kleene
    , chain
    , anyOf
    , allOf
    , count
    , string
    , oneOf
    , set
    , nTimes
    , trie
    , intersperse
    , optionallyThen
    , any
    , noneOf
    ) where

import Prelude hiding (any)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Maybe

import Automata.Utils (fix, setConcat)

type StateLabel = Int

--based on dragon book p.147

-- NFA type parameter 'a' is the type of the alphabet
-- Set of states in the NFA is the transitive closure of (transition start)

-- typeclass?

data NFA stateLabel a = NFA { transition :: stateLabel -> Maybe a -> Set.Set stateLabel
                            , start :: stateLabel
                            , accepting :: stateLabel -> Bool
                            }


closure :: Ord stateLabel => Set.Set stateLabel -> (stateLabel -> Set.Set stateLabel) -> Set.Set stateLabel
closure initStates transformer
     = fix liftTransformer initStates
         where liftTransformer states = states `Set.union` setConcat (Set.map transformer states)

-- Not as fast as it could be, because we repeatedly follow the transitions on
-- states already in the closure, instead of just chasing newly discovered 
-- transitions.
epsilonClosure nfa states = closure states (epsilonTransitions nfa)
epsilonTransitions nfa state = transition nfa state Nothing

-- | For some 'nfa', starting with some set of 'states',
-- take the epsilon-closure of 'states', 
-- return the set of states reached by following 'input' from the epsilon-closure.
-- Note that we don't take the epsilon-closure of the result!
-- Written so as to be foldl-able.
next nfa states input = setConcat $ Set.map (\state -> transition nfa state (Just input)) possibleCurrentStates
    where possibleCurrentStates = epsilonClosure nfa states

type MapNFATransitionMap a = Map.Map (StateLabel, Maybe a) (Set.Set StateLabel)                       
data MapNFA a = MapNFA { transitionMap :: MapNFATransitionMap a
                       , startKey :: StateLabel
                       , acceptKeys :: Set.Set StateLabel
                       } deriving (Show)
                       
-- | Lift a pragmatic MapNFA to a theoretically more pure NFA
mapNFAToNFA :: Ord a => MapNFA a -> NFA StateLabel a
mapNFAToNFA (MapNFA t s a) = NFA (transitionMapToFunction t) s (`Set.member` a)
     where transitionMapToFunction m = curry $ fromMaybe Set.empty . (`Map.lookup` m)

-- | Return the set of states in an NFA.
states (MapNFA m s _)= Set.unions [keyStates, valueStates, Set.singleton s]
    where keyStates = Set.map fst (Map.keysSet m)
          valueStates = setConcat $ Set.fromList (Map.elems m)

-- This starts at start states and works forward.
-- If no accepting state reached the whole NFA is dead.
-- 
-- Should instead start at accepting states and work backward, 
-- thereby eliminating "dead end" states. 
-- If no start state reached the whole NFA is dead.
reachableStates nfa = closure (Set.singleton $ startKey nfa) allNexts
    where allNexts state = Set.unions $ Map.elems (Map.filterWithKey (isFrom state) (transitionMap nfa))

unreachableStates nfa = Set.difference (states nfa) (reachableStates nfa)

-- | Clean out unreachable states from an NFA.
removeUnreachableStates nfa@(MapNFA t s a)
    = MapNFA (Map.map killUnreachables (Map.filterWithKey isFromReachable t)) s (killUnreachables a)
       where isFromReachable (state, _) _ = state `Set.member` reachableStates nfa
             killUnreachables = (`Set.difference` unreachableStates nfa)
          
-- mapkeys and mapKeysMonotonic don't have compatible types we can't 
-- parameterize over them. Annoying.
-- | Map a function over the states of an NFA.
mapStateLabels fn (MapNFA t s a)
    = MapNFA { transitionMap = Map.map (Set.map fn) (Map.mapKeys fnKeys t)
             , startKey = fn s
             , acceptKeys = Set.map fn a
             }
        where fnKeys (keyStateLabel, a) = (fn keyStateLabel, a)

-- | Make the state labels of two NFAs combineable.
compatibleWRT refNFA = mapStateLabels (+ (1 + Set.findMax (states refNFA)))
freshStateLabels nfa = [1 + Set.findMax (states nfa) .. ]

-- | Predicate for Map.filterWithKey to filter an NFA transitionMap,
-- keeping transitions that originate from 'from'.
isFrom :: StateLabel -> (StateLabel, a) -> Set.Set StateLabel -> Bool
isFrom from (candidate, _) _ = from == candidate
                             
copyStateTransitionsToMany from to m = Set.fold (copyStateTransitions from) m to 

copyStateTransitions from to m 
     = let fromTransitionMap = Map.filterWithKey (isFrom from) m
           toTransitionMap = Map.mapKeys (tmap (const to, id)) fromTransitionMap
-- Overwrite the "from" part of the transition  ^^^^^^^^  ^^                                           
               where tmap (f,g) (a,b) = (f a, g b) -- "map over tuples"
       in Map.unionWith Set.union toTransitionMap m 

-- | Move state transitions going to 'from' to 'to' in 'm'
redirectInboundStateTransitions :: StateLabel -> StateLabel -> MapNFATransitionMap a
                                -> MapNFATransitionMap a
redirectInboundStateTransitions from to m
    = let rewrite = (\target -> if target == from then to else target)
          rewriteSet = Set.map rewrite
      in  Map.map rewriteSet m

-- | Remove state transitions originating from 'from'
removeStateTransitions :: Ord a => StateLabel -> MapNFATransitionMap a
                       -> MapNFATransitionMap a
removeStateTransitions from
    = Map.filterWithKey ((not .) . isFrom from)

-- Return a set of relevant inputs from some stateSet in a MapNFA transition map
relevantInputsFrom stateSet (MapNFA m _ _) = 
    let keys = Map.keysSet m
        fromAnyOfS = Set.filter (\(label, ma) -> label `Set.member` stateSet) keys
        realTransitions = Set.filter (\(_, ma) -> isJust ma) fromAnyOfS
    in Set.map (\(_, Just a) -> a) realTransitions

----------------------------
--- MapNFA contructors
-- See Dragon Book p.159-161. Based on Algorithm 3.23: McNaughton-Yamada-Thompson

-- | NFA that matches the empty string only.
empty :: Ord a => MapNFA a
empty = MapNFA { transitionMap = Map.fromList [((0, Nothing), Set.singleton 1)]
               , startKey = 0
               , acceptKeys = Set.singleton 1
               }
-- | NFA that matches the provided item only.   
singleton :: Ord a => a -> MapNFA a
singleton a = MapNFA { transitionMap = Map.fromList [ ((0, Just a), Set.singleton 1) ]
                     , startKey = 0
                     , acceptKeys = Set.singleton 1
                     }

-- first's start state is the start state
-- first's accepting states are no longer accepting
-- Copy second's start state transitions into first's accepting states.
-- | NFA that matches the language of the first NFA followed by the second's.
catenate :: Ord a => MapNFA a -> MapNFA a -> MapNFA a
catenate first@(MapNFA firstTransitions firstStart firstAccepting) second
         = case compatibleWRT first second of
             (MapNFA secondTransitions secondStart secondAccepting) ->
               MapNFA { transitionMap = copyStateTransitionsToMany secondStart firstAccepting bothMaps
                      , startKey = firstStart
                      , acceptKeys = secondAccepting
                      }
                 where bothMaps = Map.unionWith Set.union firstTransitions secondTransitions

-- Introduce new start and accepting states
-- Create epsilon transitions from new start to input start states
-- Create epsilon transitions from input accepting states to new accepting state
-- | NFA that matches the language of either provided NFA.
alternate :: Ord a => MapNFA a -> MapNFA a -> MapNFA a
alternate first@(MapNFA firstTransitions firstStart firstAccepting) second
    = case compatibleWRT first second of
        newSecond@(MapNFA secondTransitions secondStart secondAccepting) ->
            MapNFA { transitionMap = Map.unionsWith Set.union [bothMaps, newStartTransitions, newAcceptTransitions]
                   , startKey = newStart
                   , acceptKeys = Set.singleton newAccept
                   }
              where bothMaps = Map.unionWith Set.union firstTransitions secondTransitions
                    [newStart, newAccept] = take 2 (freshStateLabels newSecond)
                    newStartTransitions = Map.fromList [((newStart, Nothing), Set.fromList [firstStart, secondStart])]
                    newAcceptTransitions = Map.fromList $ Set.elems $ Set.map epsilonToNewAccept (firstAccepting `Set.union` secondAccepting)
                    epsilonToNewAccept s = ((s, Nothing), Set.singleton newAccept)

-- Add a new start state. 
-- Add an epsilon transition from the new start state to the old start state.
-- Add a new accepting state.
-- Add epsilon transitions from each old accepting state to the new accepting state.
-- Add an epsilon transition from the new start state to the new accepting state.
-- TODO Remove duplication with 'alternate'.
-- | NFA that matches the provided NFA or the empty string.
optional :: Ord a => MapNFA a -> MapNFA a
optional nfa@(MapNFA t s a) 
    = MapNFA { transitionMap = Map.unionsWith Set.union [t, newStartTransitions, newAcceptTransitions]
             , startKey = newStart
             , acceptKeys = Set.singleton newAccept
             }
        where [newStart, newAccept] = take 2 (freshStateLabels nfa)
              newStartTransitions = Map.fromList [((newStart, Nothing), Set.fromList [s, newAccept])]
              newAcceptTransitions = Map.fromList $ map epsilonToNewAccept (Set.elems a)
              epsilonToNewAccept s = ((s, Nothing), Set.singleton newAccept)
         
-- Epsilon transitions from all accepting states to start state.
-- | NFA that matches the provided NFA's language repeatedly.
loop :: Ord a => MapNFA a -> MapNFA a
loop (MapNFA t s a)
    = MapNFA { transitionMap = t `Map.union` epsilons
             , startKey = s
             , acceptKeys = a
             }
        where epsilons = Map.fromList $ map (\a -> ((a, Nothing), Set.singleton s)) (Set.elems a)
        
-- * Higher order combinators!
-- | Kleene closure of an NFA
kleene :: Ord a => MapNFA a -> MapNFA a
kleene = optional . loop

--On NFAs
-- | Matches the language of each provided NFA in turn.
chain :: Ord a => [MapNFA a] -> MapNFA a
chain = foldr1 catenate

-- | Matches the language of any of the provided NFAs.
anyOf :: Ord a => [MapNFA a] -> MapNFA a
anyOf = foldr1 alternate

-- | Matches the languages of the provided NFAs in any order, 
-- with no repetition.
allOf :: Ord a => [MapNFA a] -> MapNFA a
allOf nfas = anyOf $ map chain (List.permutations nfas)

-- | Matches the language of the provided NFA 'n' times.
count :: Ord a => Int -> MapNFA a -> MapNFA a
count n dfa = chain $ replicate n dfa

-- On elements
lift :: Ord a => [a] -> [MapNFA a]
lift = map singleton

-- | NFA that matches the provided sequence in order.
string :: Ord a => [a] -> MapNFA a
string = chain . lift

-- useful for e.g. regex character classes
-- | NFA that matches any item in the provided list.
oneOf :: Ord a => [a] -> MapNFA a
oneOf = anyOf . lift

-- | NFA that matches the provided items in any order, 
-- with no repetition.
set :: Ord a => [a] -> MapNFA a
set = allOf . lift

-- | NFA that matches a sequence of 'n' repetitions of 
-- the provided item.
nTimes :: Ord a => Int -> a -> MapNFA a
nTimes n ch = string (replicate n ch)

-- Why not?
-- | Efficiently matches any of the provided sequences.
trie :: Ord a => [[a]] -> MapNFA a
trie strings = anyOf $ map string strings

-- | Efficiently matches the sequence of 'nfas', each separated by 
-- the language of 'middle'.
intersperse :: Ord a => MapNFA a -> [MapNFA a] -> MapNFA a 
intersperse middle nfas = chain $ List.intersperse middle nfas

-- | Matches the language of the provided 'nfa', optionally preceded
-- by language of 'optionalNFA'.
optionallyThen :: Ord a => MapNFA a -> MapNFA a -> MapNFA a
optionallyThen optionalNFA nfa = optional optionalNFA `catenate` nfa


-- | NFA that matches any single item.
-- Much like the regex . operator
any :: (Ord a, Enum a, Bounded a) => MapNFA a
any = oneOf [minBound .. maxBound]

-- | NFA that matches any item except those provided.
-- Much like a regex negated character class.
noneOf :: (Bounded a, Enum a, Ord a) => [a] -> MapNFA a
noneOf xs = oneOf $ [minBound .. maxBound] List.\\ xs
          