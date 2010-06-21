module Main where

import Automata.DFA.Datatype as DFA
import Data.Set
import System.Environment
import Control.Monad

main = do
    inpStr <- readFile "test"
    let inp = read inpStr :: [String]
    testSize <- liftM (read . (!!0)) $ getArgs :: IO Int
    putStrLn $ show $ size $ DFA.states $ buildDictionary $ take testSize inp
