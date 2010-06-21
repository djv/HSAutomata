{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Operator precedence parser for regular expressions.
module Automata.RegularExpression.Parser (parse, eParse) where

import Automata.RegularExpression.Datatype
import Automata.RegularExpression.Monoid

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as Map
import Maybe

newtype ParserStack a = ParserStack { unParserStack :: StateT String (ErrorT String Identity) a }
    deriving (Monad, MonadState String)

type Parser = ParserStack (RegularExpression Char)

runParserStack stack input = runIdentity $ runErrorT $ (\a -> runStateT a input) $ unParserStack stack


-- | Parse a string representing a regular expression. 
-- Return either an error message or the resulting
-- RegularExpression over Char.
parse :: String -> Either String (RegularExpression Char)
parse input = do (result, []) <- runParserStack parseTop input
                 return result

-- | Parse a string representing a regular expression.
-- Return the resulting RegularExpression over Char.
-- Throw an exception if the string is invalid.
eParse :: String -> RegularExpression Char                 
eParse input = case (parse input) of
                 (Right result) -> result

-- TODO Tokenize the regular expression, permitting multi-char operators, 
-- e.g. allow escaping special characters, *? as a single operator
         
parseTop :: Parser
parseTop = parseExpression 0

parseExpression :: Int -> Parser
parseExpression minPrecedence
    = do input <- get
         case input of 
            []        -> return Empty
            (op:rest) -> do put rest
                            left <- parseStandalone op 
                            parseLeftAssociatives left minPrecedence

         
-- Standalone Parser <==> Pratt-Crockford 'nud'
-- Tokens that don't care about what's left of them.         
parseStandalone :: Char -> Parser
parseStandalone '(' = do result <- parseExpression 0
                         (')':rest) <- get
                         put rest
                         return result

-- These error cases are places where special characters are used when literals are expected
-- We could just accept them as literals, since it is unambiguous
-- That would permit the string "|" or "*" as a regular expression, for example
-- But I think, it's better to enforce some consistency
-- If you want to use special characters, you always spell them the same way.
parseStandalone '*' = fail "unexpected *" 
parseStandalone ')' = fail "unexpected )"
parseStandalone '|' = fail "unexpected |"
parseStandalone lit = return $ singleton lit


-- Left associative parser <==> Pratt-Crockford 'led'
-- Tokens that care what's left of them
parseLeftAssociatives :: RegularExpression Char -> Int -> Parser
parseLeftAssociatives left minPrecedence 
    = do input <- get
         case input of 
             (t:rest) -> if minPrecedence < getPrecedence t
                             then do put rest
                                     left' <- parseLeftBindingOp t left
                                     parseLeftAssociatives left' minPrecedence
                             else return left
             [] -> return left

-- Keep in sync with PrettyPretter.hs
getPrecedence '*' = 30
getPrecedence '|' = 10
getPrecedence ')' = 0
getPrecedence _   = 20 

parseLeftBindingOp '*' left = return $ kleene left
parseLeftBindingOp '|' left = do right <- parseExpression (getPrecedence '|')
                                 return $ alternation left right

-- Catenation is an implicit operator, so we've been handed a literal that has already been consumed.
-- We want to pretend we've been handed a catenation operator that has already been consumed.
-- Thus we put back the literal before resuming parsing.
parseLeftBindingOp lit left = do input <- get
                                 put (lit:input)
                                 right <- parseExpression (getPrecedence lit)
                                 return $ catenation left right
{-
Traditional regex precedence:
    *,              binds left, most tightly
    concatenation,  associative
    |,              associative, least tightly  
    
    ab*|c*d == a(b*)|(c*)d == (a(b*))|((c*)d)

-}     

