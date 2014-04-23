import System.Environment
import System.Process
import System.FilePath
import System.Directory
import Control.Exception

-- Data Types
data Document = Document { path :: String
                         , name :: String
                         , folderPath :: String
                         } deriving Show

type Job = Document -> IO String

-- Main
main = do
        -- Gets the "list" of documents from the command line
        args <- getArgs 
        let docPath = head args -- Ok maybe just the first one at the moment
        let docName = dropExtension docPath
        let folderPath = takeDirectory docPath
        let document = Document docPath docName folderPath
        -- Parses the header of the current file
        headerLineList <- readHeader document 
        let jobList = parseHeader headerLineList
        -- Run each job read from the document header
        mapM (\func -> func document) jobList
        -- Main print the last IO operation so we give it a small one
        return ()

-- Header Parser
readHeader :: Document -> IO [String]
readHeader doc = do
                    -- NOTE how loose should we be with the formatting of the header
                    fileAsString <- readFile $ path doc 
                    let fileAsList = lines fileAsString
                    return $ takeWhile (\line -> line /= "") fileAsList

parseHeader :: [String] -> [Job]
parseHeader headerLines = reverse $ foldl buildJobs [] headerLines
    where buildJobs = (\acc line -> (buildJob $ words $ line) : acc)

buildJob :: [String] -> Job
buildJob ["%command", command] = commandJob command
buildJob ("%clean":suffixes) = cleanJob suffixes
buildJob _ = error "BuildJob: a header line did not match any jobs"
                       
-- Job Functions
commandJob :: String -> Document -> IO String
commandJob command doc = readProcess command [name doc] []

cleanJob :: [String] -> Document -> IO String -- The return is added for a uniform interface
cleanJob suffixToDelete doc = mapM_  prependPath suffixToDelete >> return ""
    where prependPath = (\elem -> removeFile $ (name doc) ++ "." ++ elem) 

-- Exception handling
handleIOException :: IOException -> IO String
handleIOException ioex = return $ show ioex
