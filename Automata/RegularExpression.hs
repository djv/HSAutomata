-- | Definitions and functions related to regular expressions. 
-- A regular expression is a theoretical computer science construct
-- that specifies a language over strings. 
-- Regular expressions are usually written with a syntax such as
-- @\"(ab)*|ac|bc\"@.
module Automata.RegularExpression (
      module Automata.RegularExpression.Datatype
    , module Automata.RegularExpression.Functor
    , module Automata.RegularExpression.Monoid
    , module Automata.RegularExpression.Parser
    , module Automata.RegularExpression.PrettyPrinter
    ) where

import Automata.RegularExpression.Datatype 
import Automata.RegularExpression.Functor
import Automata.RegularExpression.Monoid
import Automata.RegularExpression.Parser
import Automata.RegularExpression.PrettyPrinter