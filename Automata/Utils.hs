-- | Utilities that belong in modules not in our control.
module Automata.Utils (fix, setConcat, removeDuplicates) where


import qualified Data.List as List
import Data.Set    
import qualified Data.Map as Map
    
fix f x = if x == x' 
             then x 
             else fix f x'
    where x' = f x

setConcat :: Ord a => Set (Set a) -> Set a
setConcat = fold union empty 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = List.nub  -- nub is a bad name