module File
( buildJobList
) where

-- Project Level Imports --
import Jobs (buildJob)
import Macros (expandMacros)
import Datatypes (Document (..), Job (..))

-- Global Level Imports --
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Control.Monad (liftM)
import System.IO (withFile, IOMode(ReadMode), hGetContents)

-- Public Functions --
buildJobList :: Document -> IO [Job]
buildJobList doc = liftM (parseHeader . expandMacros doc) $ readHeader $ path doc

-- Header Parsing Functions --
readHeader :: String -> IO [String]
readHeader docPath = withFile docPath ReadMode $ \docFile -> do
                       -- readFile is not used to ensure the file handle is closed
                       docStr <- hGetContents docFile
                       let header = takeHeader docStr
                       header `seq` return header
    where takeHeader = takeWhile couldBeHeader . rstripWhitespace . lines
          couldBeHeader line = foldl (\acc job -> job `isPrefixOf` line || acc)
            -- Must contain all types of jobs and macro any could appear in the header
            False ["%command",  "%link", "%link-doc", "%clean", "%macro"]

parseHeader :: [String] -> [Job]
parseHeader headerLines = reverse $ foldl buildJobs [] headerLines
    where buildJobs acc line = buildJob (words line) : acc

-- Helper Functions --
rstripWhitespace :: [String] -> [String]
rstripWhitespace = map $ reverse . dropWhile isSpace . reverse
