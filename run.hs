module Main where

import Automata.DFA.Datatype as DFA
import Data.Set
import System.Environment
import Control.Monad
import qualified Data.ByteString.Char8 as B

main = do
    inpStr <- B.readFile "test"
    let inp = B.lines inpStr :: [B.ByteString]
    testSize <- liftM (read . (!!0)) $ getArgs :: IO Int
    putStrLn $ show $ stateSize $ buildDictionary $ take testSize inp
