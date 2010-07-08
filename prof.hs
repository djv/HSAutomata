module Main where

import Automata.DFA.MyDFA as MDFA
import Data.Set
import System.Environment
import System.IO
import Data.Time.Clock
import Control.Monad
import qualified Data.ByteString.Char8 as B
import List

main = do
    args <- getArgs
    inpStr <- B.readFile $ args !! 0
    let inp = B.lines inpStr :: [B.ByteString]
    let testSize = read $ args !! 1 :: Int
    let dfa = determinize . buildTrie $ take testSize inp
    --putStrLn . show . stateSizeReal . determinize . buildTrie $ take testSize inp
    print . length {-. nub . sort-} $ matchRegex dfa (parseRegex "2*.a.b.c.2*")
