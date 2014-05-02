module Structures where

import System.Environment
import Data.Char
import Data.String
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

--    In the first section, we define some parsers for the various
-- types that we want to extract from the input file.
--    Their names and structure should make it clear what each of 
-- them is parsing.

spaces :: Parser ()
spaces = skipMany1 space

-- In this case, strings are only used to define names of propostions.
parseString :: Parser String
parseString = do
                first <- letter <|> char '-' --negation
                x <- many(letter <|> digit)
                return $ (first:x)

parseDouble :: Parser Double
parseDouble = do
                  zero <- char '0'
                  decDot <- char '.'
                  value <- many(digit)
                  return $ (read (zero:([decDot]++value)) :: Double)
                  
parseList :: Parser [String]
parseList = do
               c0 <- skipMany letter
               c1 <- skipMany space
               c2 <- char '[' >> many (char ' ')
               x <- parseString `sepBy` (many (char ' '))
               c3 <- char ']'
               return $ x

parseEmptyList :: Parser [String]
parseEmptyList = do
               c0 <- skipMany letter
               c1 <- skipMany space
               c2 <- char '[' >> many (char ' ')
               c3 <- char ']'
               return []

parseWildcard :: Parser String
parseWildcard = do
                c <- char '_'
                return [c]

parseArguments :: Parser ([String], [String], String, Double)
parseArguments = do
               premises <- parseList <|> parseEmptyList
               exceptions <- parseList <|> parseEmptyList
               c0 <- skipMany1 space
               conclusion <- parseString
               c1 <- skipMany1 space
               weight <- parseDouble
               return (premises, exceptions, conclusion, weight)

parseStandard :: Parser (String, String)
parseStandard = do
                c0 <- (skipMany1 letter) >> (skipMany1 space)
                prop <- parseString <|> parseWildcard
                c1 <- skipMany1 space
                name <- many (letter <|> char '_')
                return (prop, name)

-- Parses both acceptability and applicability queries, since they
--have the same simple format.
parseQuery :: Parser String
parseQuery = do
                c0 <- (skipMany1 letter) >> (skipMany1 space)
                s <- parseString
                return s


--   These functions are used to process the input file and parse
-- them using the parsers defined above. They can raise an error
-- if the parser fails. This is helpful, since these functions only
-- get to process the subset of the input which the user has identified
-- as belonging to the type with which each parser is concerned.

readArguments :: String -> ([String], [String], String, Double)
readArguments input = case parse parseArguments "" input of
  Left err -> error ("Argument synatx error> " ++ input)
  Right val -> val

readAssumptions :: String -> [String]
readAssumptions input = case parse (parseList <|> parseEmptyList) "" input of
  Left err -> error ("Assumptions syntax error> " ++ input)
  Right val -> val  

readStandard :: String -> (String, String)
readStandard input = case parse parseStandard "" input of
    Left err -> error ("Proof standard assignment error> " ++ input)
    Right val -> val

readQuery :: String -> String
readQuery input = case parse parseQuery "" input of 
    Left err -> error ("Query syntax error> " ++ input)
    Right val -> val


-- readProp was only used as a test parser, but could be used to parser
-- propositions only.
readProp :: String -> String
readProp input = case parse (parseString) "" input of
  Left err -> ("No match: " ++ show err)
  Right val -> val