module File
( buildJobList
) where

-- Project Level Imports --
import Jobs (buildJob)
import Macros (expandMacros)
import Datatypes (Document (..), Job (..))

-- Global Level Imports --
import Data.Char (isSpace)
import Control.Monad (liftM)

-- Public Functions --
buildJobList :: Document -> IO [Job]
buildJobList doc = liftM (parseHeader . expandMacros doc) $ readHeader $ path doc

-- Header Parsing Functions --
readHeader :: String -> IO [String]
readHeader docPath = liftM takeHeader $ readFile docPath
    where takeHeader = takeWhile couldBeHeader . rstripWhitespace . lines
          couldBeHeader line = not (null line) && head line == '%'

parseHeader :: [String] -> [Job]
parseHeader headerLines = reverse $ foldl buildJobs [] headerLines
    where buildJobs acc line = buildJob (words line) : acc

-- Helper Functions --
rstripWhitespace :: [String] -> [String]
rstripWhitespace = map $ reverse . dropWhile isSpace . reverse
