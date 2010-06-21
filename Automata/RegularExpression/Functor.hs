-- | Functor instance for RegularExpression. 
-- Use fmap to transform a RegularExpression over Char into a 
-- RegularExpression over some other type.

module Automata.RegularExpression.Functor (Functor) where

import Automata.RegularExpression.Datatype

instance Functor RegularExpression where 
    f `fmap` Empty             = Empty
    f `fmap` (Singleton a)     = singleton (f a)
    f `fmap` (Kleene a)        = kleene (f `fmap` a)
    f `fmap` (Catenation a b)  = catenation (f `fmap` a) (f `fmap` b)
    f `fmap` (Alternation a b) = alternation (f `fmap` a) (f `fmap` b)

