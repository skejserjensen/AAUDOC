import System.Environment
import System.FilePath

import File

-- Main
main = do
        -- Gets the "list" of documents from the command line
        args <- getArgs 
        let docPath = head args -- Ok maybe just the first one at the moment
        let docName = dropExtension docPath
        let folderPath = takeDirectory docPath
        let document = Document docPath docName folderPath
        -- Parses the header of the current file
        jobList <- buildJobList document
        -- Run each job read from the document header
        mapM (\func -> func document) jobList
        -- Main print the last IO operation so we give it a small one
        return ()

