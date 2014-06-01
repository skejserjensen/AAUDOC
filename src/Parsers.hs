module Parsers
( addJobOutputParser
) where

-- Project Level Imports --
import Datatypes (Job (..))

-- Global Level Imports --
import Control.Monad ((>=>))
import System.Exit (ExitCode (..))
import Data.List (isPrefixOf)

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
    where stdoutErrors = unlines $ takeTeXError [] $ reverse $ lines stdout

-- Helper Functions --
takeTeXError :: [String] -> [String] -> [String]
takeTeXError acc [] = acc -- A rough match of the parentheses structure used by LaTeX for output
takeTeXError acc (x : xs) = if (") (" `isPrefixOf` x || "(" `isPrefixOf` x) && lineIsTexFile x
                            then takeTeXError (x : acc) [] else takeTeXError (x : acc) xs
    where lineIsTexFile line = ".tex" `isPrefixOf` dropWhile (/= '.') (dropWhile (/= '/') line)
