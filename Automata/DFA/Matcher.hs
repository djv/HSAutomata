module Automata.DFA.Matcher (matches) where

import Control.Monad
import Data.Set
import Maybe
import Data.Monoid
import Automata.DFA.Datatype (DFA(..),DTransducer(..), buildDictionary, mapDFAToDFA)

-- | A DFA 'matches' a finite list of input 
-- iff after following a transition for each item in the list, 
-- the resulting state is an accepting state.
matches :: DFA stateLabel a -> [a] -> Bool
matches (DFA transition start accepting) input = matches' input start
    where matches' input state 
           = maybe False 
                   accepting 
                   (foldM transition state input)

transduce :: Monoid b => DTransducer st a b -> [a] -> b
transduce (DTransducer trans start) input =
    let
        init = (start, mempty)
        transition' (state, out) input =
            trans state input >>= (\(st,b) ->
                                       return (st, out `mappend` b))
    in
      case foldM transition' init input of
        Just (_, out) -> out
        Nothing       -> mempty

data Accept = Unit | Accept | Reject
instance Monoid Accept where
    mempty = Unit
    mappend x y = case y of
                    Unit -> x
                    _    -> y

type TransDFA st a = DTransducer st a Accept

transMatches :: TransDFA st a -> [a] -> Bool
transMatches transducer input =
    case transduce transducer input of
      Accept -> True
      Reject -> False
      Unit   -> error "What to do with the empty word?"
