module Main where

import Automata.DFA.MyDFA as MDFA
import Data.Set
import System.Environment
import Control.Monad
import qualified Data.ByteString.Char8 as B

main = do
    inpStr <- B.readFile =<< (liftM (!!1) $ getArgs)
    let inp = B.lines inpStr :: [B.ByteString]
    testSize <- liftM (read . (!!0)) $ getArgs :: IO Int
    --putStrLn $ show $ stateSize $ buildTrie $ take testSize inp
    putStrLn . show . stateSize . determinize . buildTrie $ take testSize inp
