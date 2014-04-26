module File
( Document(..)
, buildJobList
) where

import Data.Char
import System.Process
import System.Directory
import Control.Exception

-- Data Types
data Document = Document { path :: String
                         , name :: String
                         , folderPath :: String
                         } deriving Show

type Job = Document -> IO String

-- Public Functions --
buildJobList :: Document -> IO [Job]
buildJobList doc = readHeader (path doc) >>= return . parseHeader


-- Header Parser --
readHeader :: String -> IO [String]
readHeader docPath = readFile docPath >>= return . takeHeader
    where takeHeader = takeWhile (/= "") . rstripWhitespace . lines

parseHeader :: [String] -> [Job]
parseHeader headerLines = reverse $ foldl buildJobs [] headerLines
    where buildJobs = (\acc line -> (buildJob $ words $ line) : acc)


-- Job Creation --
buildJob :: [String] -> Job
buildJob ["%command", command] = commandJob command
buildJob ("%clean":suffixes) = cleanJob suffixes
buildJob list = error $ "BuildJob: unknown operation in header: " ++ show list
                
       
-- Job Functions --
commandJob :: String -> Document -> IO String
commandJob command doc = readProcess command [name doc] []

cleanJob :: [String] -> Document -> IO String 
-- The return of "" is added for a uniform interface between all jobs
cleanJob suffixToDelete doc = mapM_  prependPath suffixToDelete >> return ""
    where prependPath = (\elem -> removeFile $ (name doc) ++ "." ++ elem) 


-- Helper Functions
rstripWhitespace :: [String] -> [String]
rstripWhitespace listOfStrings = map rstrip listOfStrings 
    where  rstrip = reverse . dropWhile isSpace . reverse
