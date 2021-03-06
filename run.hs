module Main where

import Automata.DFA.MyDFA as MDFA
import Data.Set
import System.Environment
import System.IO
import Data.Time.Clock
import Control.Monad
import qualified Data.ByteString.Char8 as B
import List
--import Test.BenchPress

main = --bench 5 $
    do
    args <- getArgs
    inpStr <- B.readFile $ args !! 0
    let inp = B.lines inpStr :: [B.ByteString]
    let testSize = read $ args !! 1 :: Int
    let dfa = determinize . buildTrie $ take testSize inp
    --putStrLn . show . stateSizeReal . determinize . buildTrie $ take testSize inp
    loop dfa

loop dfa = do
    putStr "> "
    hFlush stdout
    inp <- getLine
    case inp of
         "q" -> return ()
         otherwise -> do 
            start <- getCurrentTime
            print . length {-. nub . sort-} $ matchRegex dfa (parseRegex inp)
            finish <- getCurrentTime
            print $ diffUTCTime finish start
            loop dfa

anySym = Sum [Symbol 'a', Symbol 'b', Symbol 'c']
