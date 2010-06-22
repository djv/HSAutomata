-- | A monoid instance for regular expressions, in which @++@ is 
-- regular expression catenation.
module Automata.RegularExpression.Monoid where

import Data.Monoid
import Automata.RegularExpression.Datatype

instance Monoid (RegularExpression a) where
    mempty = Empty
    mappend = catenation

