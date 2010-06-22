-- | Pretty-print @RegularExpression@, minimizing the use of parens.
-- @toString@ on a @(RegularExpression Char)@ should satisfy
-- toString $ parse $ toString $ parse s == s.
module Automata.RegularExpression.PrettyPrinter (toString) where

import Automata.RegularExpression.Datatype

-- Precedence-aware pretty-printing, keep in sync with Parser.hs!
paren s = "(" ++ s ++ ")"
toString = toString' 0
toString' _ Empty = ""
toString' n (Singleton a) = [a]
toString' n (Kleene a) = toString' 30 a ++ "*"
                 
toString' n (Catenation a b) | n > 20 = paren sub
                             | otherwise = sub
    where sub = toString' 20 a ++ toString' 20 b
                       
toString' n (Alternation a b) | n > 10 = paren sub
                              | otherwise = sub
    where sub = toString' 10 a ++ "|" ++ toString' 10 b
                       