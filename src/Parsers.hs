module Parsers
( addJobOutputParser
) where

-- Project Level Imports --
import Datatypes (Job (..))

-- Global Level Imports --
import Control.Monad ((>=>))
import Data.List (isPrefixOf, isInfixOf)
import System.Exit (ExitCode (..))

-- Public Functions --
addJobOutputParser :: Job -> Job
addJobOutputParser (StandardJob operation function) = StandardJob operation function
addJobOutputParser (CommandJob command operation function)
            | command `elem` ["latex", "pdflatex", "xelatex", "lualatex"] =
                            CommandJob command operation $ function >=> laTeXStripNonErrors
            | otherwise = CommandJob command operation function

-- Job Output Parser Functions --
laTeXStripNonErrors :: (ExitCode, String, String) -> IO (ExitCode, String, String)
laTeXStripNonErrors (ExitSuccess, stdout, stderr) = return (ExitSuccess, stdout, stderr)
laTeXStripNonErrors (exitCode, stdout, stderr) = return (exitCode, stdoutErrors, stderr)
    where stdoutErrors = unlines $ takeTeXError [] "" $ reverse $ lines stdout

-- Helper Functions --
takeTeXError :: [String] -> String -> [String] -> [String]
takeTeXError acc _ [] = acc
takeTeXError acc _ ("" : xs) = takeTeXError ("" : acc) "" xs
takeTeXError acc tmp (x : xs) = if couldBeTexFile x && lineIsTexFile (x ++ tmp)
                                then takeTeXError (x : acc) "" []
                                else takeTeXError (x : acc) (x ++ tmp) xs
    where couldBeTexFile line = "(" `isPrefixOf` line || ") (" `isPrefixOf` line
          lineIsTexFile line = ".tex" `isInfixOf` dropWhile (/= '.') line
