module Main where

import Automata.DFA.MyDFA as MDFA
import Data.Set
import System.Environment
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Test.BenchPress

main = bench 4 $
    do
    inpStr <- B.readFile =<< (liftM (!!1) $ getArgs)
    let inp = B.lines inpStr :: [B.ByteString]
    testSize <- liftM (read . (!!0)) $ getArgs :: IO Int
    --putStrLn . show . stateSizeReal . determinize . buildTrie $ take testSize inp
    (stateSize . determinize . buildTrie $ take testSize inp) `seq` return ()
