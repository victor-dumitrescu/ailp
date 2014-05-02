\begin{code}
module Main where

import System.IO
import ParseInput
import CarneadesDSL
import Data.Maybe
import Control.Monad

getFileName :: IO String
getFileName =
  do putStrLn "Enter the name of the input file."
     fn <- getLine
     text <- readCAESFile fn
     return text

--Returns the contents of the input file as an IO String.
readCAESFile :: String ->  IO String
readCAESFile fn = do
            caesIn <- openFile fn ReadMode
            text <- hGetContents caesIn
            return(text)

-- Functions which do pattern matching the argument tuple returned 
--by readArguments (in module Structures.hs)
getPremises :: ([String], [String], String, Double) -> [PropLiteral]
getPremises (el, _, _, _) = map mkProp el

getExceptions :: ([String], [String], String, Double) -> [PropLiteral]
getExceptions (_, el, _, _) = map mkProp el

getConclusion :: ([String], [String], String, Double) -> PropLiteral
getConclusion (_, _, el, _) = mkProp el

getWeight :: ([String], [String], String, Double) -> Double
getWeight (_, _, _, el) = el


-- Processes the argument tuple as parsed from the input file to Carneades data types.
toArg :: ([String], [String], String, Double) -> (Argument, Double)
toArg a = (Arg (getPremises a, getExceptions a, getConclusion a), getWeight a)

-- Constructs the weight function need by the CAES.
getWeightFunction :: [(Argument, Double)] -> ArgWeight
getWeightFunction xs = f
            where f x | isJust (lookup x xs) = fromJust (lookup x xs )
                      | otherwise = error "No weight assigned." 
                      --it should never actually reach this point
                      --this would be an argument declaration error 
                      --and should be caught when parsing arguments

-- Transforms strings parsed from the input file to the corresponding proof standards defined in CarneadesDSL.lhs
toStandard :: String -> ProofStandard
toStandard x = case x of
            "scintilla" -> scintilla
            "preponderance" -> preponderance
            "clear_and_convincing" -> clear_and_convincing
            "beyond_reasonable_doubt" -> beyond_reasonable_doubt
            "dialectical_validity" -> dialectical_validity

getProp :: (String, String) -> (PropLiteral, ProofStandard)
getProp (prop, standard) = (mkProp prop, toStandard standard)

-- Identifies the default standard, if there is one. The default
--standard is declared in the input file using the wildcard _.
defaultStandard :: [(String, String)] -> Maybe ProofStandard
defaultStandard xs | null result = Nothing
                   | (length result) == 1 = Just $ toStandard $ snd $ head result
                   | otherwise = error ("Multiple default standard defined> " ++ show result)
                        where result = filter (\(x,y) -> x == "_") xs

-- Constructs the standards function required to build a CAES.
--It checks if a standard is defined for the required proposition
--and returs it. Otherwise, it returns the default standard. If
--no default standard is defined, it raises an error.
getStandardsFunction :: Maybe ProofStandard -> [(PropLiteral, ProofStandard)] -> PropStandard
getStandardsFunction d xs = f
                    where f x | isJust (lookup x xs) = fromJust (lookup x xs)
                              | otherwise = if (isJust d) then fromJust d else error ("No proof standard defined for " ++ show x)

mainIO :: IO ()
mainIO = do putStrLn "Enter the name of the input file."
            
            --  Note: Can switch between entering the input file name
            --at the terminal or having it predifined in the source.
            fileName <- getLine
            --let fileName = "example1.txt"
            
            text <- readCAESFile fileName

            --Processing arguments
            let args = map toArg (getArguments text)
            let argSet = mkArgSet $ map fst args
            let weight = getWeightFunction args
            
            putStrLn ""
            putStrLn "Arguments:"
            putStrLn $ show args

            --Processing assumptions
            let s = getAssumptions text
            let assumptions = mkAssumptions s
            let audience = (assumptions, weight) :: Audience
            
            putStrLn ""
            putStrLn "Assumptions:"
            putStrLn $ show assumptions

            --Processing standards function
            let defStandard = defaultStandard (getStandards text)
            let standard = getStandardsFunction defStandard $ map getProp (getStandards text)

            putStrLn ""
            putStrLn "Defined standards:"
            putStrLn $ show (getStandards text)

            let caes = CAES (argSet, audience, standard)

            --Processing applicability queries
            let appQueries = getQueries "applicable" text
            let appResults = appQueries `zip` map (\x -> filter (`applicable` caes) $ getArgs (mkProp x) argSet) appQueries

            putStrLn ""
            putStrLn "Applicability queiries:"
            mapM_ print appResults

            --Processing acceptability queries
            let accQueries = getQueries "acceptable" text
            let accResults = (getQueries "acceptable" text) `zip` map (\x -> acceptable (mkProp x) caes) accQueries
            
            putStrLn ""
            putStrLn "Acceptability queiries:"            
            mapM_ print accResults
            putStrLn ""
            
main = mainIO
\end{code}
   
References:

I used these online tutorials for help with some specific issues:
[1] http://book.realworldhaskell.org/read/io.html
[2] http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

For general Haskell matters, I also used:
[3] http://learnyouahaskell.com/