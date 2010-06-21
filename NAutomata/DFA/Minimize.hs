-- | State minimization for DFAs.
module Automata.DFA.Minimize (minimize) where

import Data.Function (on)
import Data.List (sort, groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Automata.DFA.Datatype
import Automata.Utils (fix, removeDuplicates)



-- See dragon book 3.9.6, p. 180
-- Two states are *distinguishable* if -any- input string causes different
--   acceptance behaviour starting from the states.
-- Minimization: Find indistinguishable states and merge them.
-- A partition is a set of groups
-- A group is a set of states
-- A partition includes all the states exactly once

-- Keep a current partition p, initially accepting and non-accepting states.
--    Take a group G of the current partition (of size > 1).
--        For all inputs a
--            If the transitions on a from the states in g fall into different groups
--                  split G s.t. s1 and s2 are in the same group iff s1(a) == s2(a)


-- Concretely, we will model a partition as a map from stateLabels to 
-- partition group labels.
-- The partition groups are representative state labels.
type Partition stateLabel = Map.Map stateLabel stateLabel
                
startingPartition dfa
 = Map.fromList (mapTo (Set.findMin accepts) accepts ++ mapTo (Set.findMin nonAccepts) nonAccepts) 
--                      ^^^^^^^^^^^^^^^^^^^^^                    ^^^^^^^^^^^^^^^^^^^^^^^^
--Works on empty sets thanks to laziness of zip/mapTo
    where accepts = acceptKeys dfa
          nonAccepts = states dfa Set.\\ accepts
          mapTo l s = zip (Set.elems s) (repeat l)

-- Get a single partition group by its representative
partitionGroup partition groupRepresentative
   = Map.filter (groupRepresentative ==) partition

-- Get all the representatives in a partition
groupRepresentatives partition
   = removeDuplicates $ Map.elems partition

-- Get all the partition groups in a partition   
partitionGroups partition
   = map (partitionGroup partition) (groupRepresentatives partition)

-- Get all the partition groups we might be able to do something about
possiblyDistinguishablePartitionGroups p
   = filter ((>1) . Map.size) (partitionGroups p)

groupRepresentative partition stateLabel 
    = partition Map.! stateLabel

distinguishOn dfa partition partitionGroup input
    = let pureDFA = mapDFAToDFA dfa
          states = Map.keys partitionGroup 
          targets = map (\state -> transition pureDFA state input) states -- [Maybe stateLabel]
          targetRepresentatives = map (\stateLabel -> do s <- stateLabel
                                                         return $ groupRepresentative partition s) 
                                      targets -- [Maybe stateLabel]
          groups = map (map snd) $ groupBy ((==) `on` fst) (sort $ zip targetRepresentatives states)
          newPartitionGroups = map mapToHead groups
          mapToHead lst = zip lst (repeat (head lst))
      in map Map.fromList newPartitionGroups
      
distinguish dfa partition partitionGroup
    = let inputsToTry = Set.elems $ relevantInputsFrom (Map.keysSet partitionGroup) dfa
          trySuccessiveInputs [] = [partitionGroup]
          trySuccessiveInputs (i:is) = let maybeDistinguished = distinguishOn dfa partition partitionGroup i
                                       in case length maybeDistinguished of
                                            1 -> trySuccessiveInputs is
                                            _ -> maybeDistinguished
      in trySuccessiveInputs inputsToTry
    
processPartitionOnce dfa partition
    = let groupsToTry = possiblyDistinguishablePartitionGroups partition
          trySuccessiveGroups [] = partition
          trySuccessiveGroups (g:gs) = let maybeDistinguished = distinguish dfa partition g
                                       in case length maybeDistinguished of 
                                            1 -> trySuccessiveGroups gs
                                            _ -> Map.unions (maybeDistinguished ++ [partition])
      in trySuccessiveGroups groupsToTry
      
minimizedPartitioning dfa = fix (processPartitionOnce dfa) (startingPartition dfa)

-- | Optimize a DFA. Remove redundant states, reorganizing as necessary.
minimize :: (Ord a, Ord stateLabel) => MapDFA stateLabel a -> MapDFA stateLabel a
minimize dfa = mapStateLabels ((minimizedPartitioning dfa) Map.!) dfa