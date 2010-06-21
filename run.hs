module Main where

import Automata.DFA.Datatype as DFA
import Data.Set
import System.Environment
import Control.Monad
import Data.List
import Test.QuickCheck.Gen

generateData n = liftM (nub . sort . concat) $ sample' $ resize n $ listOf1 $ resize 20 $ listOf1 $ choose ('a', 'c')

main = do
    inpStr <- readFile "test"
    let inp = read inpStr :: [String]
    testSize <- liftM (read . (!!0)) $ getArgs :: IO Int
    putStrLn $ show $ size $ DFA.states $ buildDictionary $ take testSize inp
