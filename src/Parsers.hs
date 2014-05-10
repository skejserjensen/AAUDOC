module Parsers
( addJobOutputParser
) where

-- Project Level Imports --
import Datatypes (Job (..))

-- Global Level Imports --
import Control.Monad ((>=>))
import System.Exit (ExitCode (..))

-- Public Functions --
addJobOutputParser :: Job -> Job
addJobOutputParser (Job jobOperation jobFunction)
            | jobOperation `elem` ["latex", "pdflatex", "xelatex", "lualatex"] =
                                        Job jobOperation $ jobFunction >=> laTeXStripNonErrors
            | otherwise = Job jobOperation jobFunction

-- Job Output Parser Functions --
laTeXStripNonErrors :: (ExitCode, String, String) -> IO (ExitCode, String, String)
laTeXStripNonErrors (ExitSuccess, stdout, stderr) = return (ExitSuccess, stdout, stderr)
laTeXStripNonErrors (exitCode, stdout, stderr) = return (exitCode, stdoutErrors, stderr)
    where stdoutErrors = unlines $ reverse $ takeWhile (not . endsWith "tex)") $ reverse $ lines stdout

-- Helper Functions --
endsWith :: Eq a => [a] -> [a] -> Bool
endsWith _ [] = False
endsWith end list = reverse end == take (length end) (reverse list)
