module Parsers
( addJobOutputParser
) where

-- Project Level Imports --
import Datatypes (Job (..))

-- Global Level Imports --
import Control.Monad ((>=>))
import System.Exit (ExitCode (..))
import Data.List (isSuffixOf)

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
    where stdoutErrors = unlines $ reverse $ takeWhile stillError $ reverse $ lines stdout
          stillError = not . endsWithElem ["tex)", "))"]

-- Helper Functions --
endsWithElem :: Eq a => [[a]] -> [a] -> Bool
endsWithElem _ [] = False
endsWithElem [] _ = False
endsWithElem (endElem : elems) list = (endElem `isSuffixOf` list) || endsWithElem elems list
