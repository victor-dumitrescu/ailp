\begin{code}

module ParseInput where

-- Functions defined in this module (except splitByLine) are used
--from the Main module and are given the whole input as a parameter.
--They use the splitByLine function to get a list of lines, filter
--them according to their scope and then parse them, using the parsers
--defined in the Structures module. Any synatx errors are raised in
--Structures. Other higher level errors are raised by these functions.

import Data.Char
import Data.List
import Data.List.Split

import Structures

-- Each function filters the lines of the input file such that the
--parsers only get to inspect the lines which begin with the relevant
--keyword.

getAssumptions :: String -> [String]
getAssumptions [] = error "No data given"
getAssumptions xs | length result > 1 = error "Only one set of assumptions needed"
                  | otherwise = head result
               where result = [readAssumptions line | line <- splitByLine xs, "assumptions" `isPrefixOf` line]
                  
getArguments :: String -> [([String], [String], String, Double)]
getArguments [] = error "No data given"
getArguments xs = [readArguments line | line <- splitByLine xs, "arg" `isPrefixOf` line] 

-- Defines the accepted names a standard can take.
standards :: [String]
standards = ["scintilla", "preponderance", "clear_and_convincing", "beyond_reasonable_doubt", "dialectical_validity"]

getStandards :: String -> [(String, String)]
getStandards [] = error "No data given"
getStandards xs | not (null errors) = error ("Not a valid standard> " ++ (show $ head errors))
                | otherwise = filter (\(x,y) -> y `elem` standards) result
            where result = [readStandard line | line <- splitByLine xs, "standard" `isPrefixOf` line]
                  errors = filter (\(x,y) -> not (elem y standards)) result

getQueries :: String -> String -> [String]
getQueries category [] = error "No data given"
getQueries category xs | not (category `elem` ["applicable", "acceptable"]) = error "No such query defined"
                       | otherwise = [readQuery line | line <- splitByLine xs, category `isPrefixOf` line]

splitByLine :: String -> [String]
splitByLine xs = wordsBy (== '\n') xs


\end{code}