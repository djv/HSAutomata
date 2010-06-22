module Automata.RegularExpression.AltParser where

import Text.ParserCombinators.Parsec

import Automata.RegularExpression.Datatype

-- By Zak Kincaid

parseFactor = between (char '(') (char ')') parseRegex
              <|> fmap singleton (noneOf "()*|")
parseKleene = do sub <- parseFactor
                 char '*'
                 return $ kleene sub
parseCatenation =
   fmap (foldl1 catenation) (many1 (try parseKleene <|> parseFactor))
parseAlternation =
   fmap (foldl1 alternation) (sepBy parseCatenation (char '|'))
parseRegex = try parseAlternation <|> parseCatenation

-- parse parseRegex "" "(ab)*|abc" :: Either ParseError (RegularExpression Char)