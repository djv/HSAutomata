-- | A representation of theoretical regular expressions, along with 
-- simplifying constructors.
module Automata.RegularExpression.Datatype (
      RegularExpression(..)
    , singleton
    , kleene
    , catenation
    , alternation
    ) where

data RegularExpression a 
    = Empty
    | Singleton a
    | Kleene (RegularExpression a)
    | Catenation (RegularExpression a) (RegularExpression a)
    | Alternation (RegularExpression a) (RegularExpression a)
    deriving (Show)


--empty = Empty
singleton = Singleton
kleene Empty = Empty
kleene o = Kleene o
catenation Empty o = o
catenation o Empty = o
catenation o1 o2 = Catenation o1 o2
alternation = Alternation

