{-# LANGUAGE TupleSections, ExistentialQuantification, BangPatterns #-}

import qualified Data.Map as M
import qualified Data.IntMap as Map
import qualified Data.Set as S
import qualified Data.IntSet as Set
import Data.List
import Data.Bits
import Data.Char

type State = Int
type StateSet = Set.IntSet
type Transitions = Map.IntMap

data MyDFA = MyDFA { transitionMap :: !(Transitions State)
                    , startKey :: !State
                    , acceptKeys :: !StateSet
                    } deriving (Show)

emptyDFA = MyDFA Map.empty 0 Set.empty

toKey :: (State, Char) -> Map.Key
toKey (s, c) = s `shiftL` 8 + ord c

insertTransition :: MyDFA -> (State, Char, State) -> MyDFA 
insertTransition dfa (s1,x,s2) = dfa {transitionMap = Map.insert (toKey (s1,x)) s2 $! (transitionMap dfa)}

main = do
    putStrLn $ show $ Map.size $ transitionMap $ foldl' insertTransition emptyDFA $ zip3 [1..300000] (cycle "abcd") [2..300001]
