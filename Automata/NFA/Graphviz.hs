module Automata.NFA.Graphviz (
      toSimpleGraphviz
    , toGraphviz
    ) where
    
import Automata.NFA.Datatype ( MapNFA(..)
                             , StateLabel
                             , states
                             )
                             
import qualified Data.Set as Set                             
import qualified Data.Map as Map

import qualified Data.Graph.Inductive.Graphviz as SimpleGraphviz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree (Gr(..))

import Data.GraphViz


toSimpleGraphviz :: (Show a, Ord a) => MapNFA a -> String
toSimpleGraphviz nfa = SimpleGraphviz.graphviz' (makeGraph nfa)

toGraphviz :: (Show a, Ord a) => MapNFA a -> String
toGraphviz = printDotGraph . toDotGraph

toDotGraph nfa = graphToDot True 
                            (makeGraph nfa) 
                            [GraphAttrs [Page (PointD 8.5 11), 
                                         Size (PointD 8.5 11),
                                         RankDir FromLeft,
                                         Landscape True,
                                         Center True]]
                            nodeToAttr 
                            edgeToAttr
    where nodeToAttr (n, a) | n `Set.member` acceptKeys nfa 
                                = [Style [SItem Bold []]]
                            | otherwise = []
          edgeToAttr (from, to, label) = [Label (StrLabel label)]

makeGraph :: (Show a, Ord a) => MapNFA a -> Gr StateLabel String 
--fix type to fix ambiguity
makeGraph nfa = mkGraph (nodes nfa) (edges nfa)
    where nodes nfa = [(a, a) | a <- Set.toList $ states nfa]
          edges nfa = Map.foldWithKey fn [] (transitionMap nfa)
            where fn (from, transition) tos accum
                     = let label = maybe "ε" show transition
                       in [(from, to, label) | to <- Set.toList tos] ++ accum