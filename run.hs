module Main where

import Automata.DFA.MyDFA as MDFA
import Data.Set
import System.Environment
import Control.Monad
import qualified Data.ByteString.Char8 as B
--import Test.BenchPress

main = --bench 5 $
    do
    inpStr <- B.getContents
    let inp = B.lines inpStr :: [B.ByteString]
    testSize <- liftM (read . (!!0)) $ getArgs :: IO Int
    --putStrLn . show . stateSizeReal . determinize . buildTrie $ take testSize inp
    putStrLn $ show $ length $ matchRegex (determinize . buildTrie $ take testSize inp) (Concat [Star anySym, Symbol 'a', Symbol 'b', Symbol 'c', Star anySym])

anySym = Sum [Symbol 'a', Symbol 'b', Symbol 'c']
