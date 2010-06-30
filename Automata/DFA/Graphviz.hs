module Automata.DFA.Graphviz (
      toGraphviz,
      mtoGraphviz,
      toSimpleGraphviz
    ) where
    
import Automata.DFA.Datatype ( MapDFA(..)
                             , StateLabel
                             , states
                             )
import qualified Automata.DFA.MyDFA as MDFA
                             
import qualified Data.Set as Set
import qualified Data.IntSet as ISet
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

import qualified Data.Graph.Inductive.Graphviz as SimpleGraphviz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree (Gr(..))

import Data.GraphViz

-- | Simple renderer to Graphviz format.
toSimpleGraphviz :: (Show a, Ord a) => MapDFA StateLabel a -> String
toSimpleGraphviz dfa = SimpleGraphviz.graphviz' (makeGraph dfa)

-- | A renderer to Graphviz format that formats the page and also 
-- marks accepting states in bold.
toGraphviz :: (Show a, Ord a) => MapDFA StateLabel a -> String
toGraphviz = printDotGraph . toDotGraph

toDotGraph dfa = graphToDot True 
                            (makeGraph dfa) 
                            [GraphAttrs [Page (PointD 8.5 11), 
                                         Size (PointD 8.5 11),
                                         RankDir FromLeft,
                                         Landscape True,
                                         Center True]]
                            nodeToAttr 
                            edgeToAttr
    where nodeToAttr (n, a) | n `Set.member` acceptKeys dfa = [Style [SItem Bold []]]
                            | otherwise = []
          edgeToAttr (from, to, label) = [Label (StrLabel (show label))]

mtoGraphviz :: MDFA.MyDFA -> String
mtoGraphviz = printDotGraph . mtoDotGraph

mtoDotGraph dfa = graphToDot True 
                            (mmakeGraph dfa) 
                            [GraphAttrs [Page (PointD 8.5 11), 
                                         Size (PointD 8.5 11),
                                         RankDir FromLeft,
                                         Landscape True,
                                         Center True]]
                            nodeToAttr 
                            edgeToAttr
    where nodeToAttr (n, a) | n `ISet.member` MDFA.acceptKeys dfa = [Style [SItem Bold []]]
                            | otherwise = []
          edgeToAttr (from, to, label) = [Label (StrLabel (show label))]

--fix type to fix ambiguity
mmakeGraph :: MDFA.MyDFA -> Gr StateLabel Char
mmakeGraph dfa = mkGraph (nodes dfa) (edges dfa)
    where nodes dfa = [(a, a) | a <- ISet.toList $ MDFA.states dfa]
          edges dfa = IMap.foldWithKey fn [] (MDFA.transitionMap dfa)
            where fn k to accum = let (from, label) = MDFA.fromKey k in (from, to, label) : accum

--fix type to fix ambiguity
makeGraph :: (Show a, Ord a) => MapDFA StateLabel a-> Gr StateLabel a 
makeGraph dfa = mkGraph (nodes dfa) (edges dfa)
    where nodes dfa = [(a, a) | a <- Set.toList $ states dfa]
          edges dfa = Map.foldWithKey fn [] (transitionMap dfa)
            where fn (from, label) to accum = (from, to, label) : accum
     