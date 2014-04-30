import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.FilePath (takeExtension, dropExtension, takeDirectory)
import System.Directory (setCurrentDirectory)

import File

-- Main Function --
main :: IO ()
main = getArgs >>= processDocuments . prepareDocumentPaths >> return ()
        where prepareDocumentPaths = map expandTexPath . filter couldBeTex 

-- Document Processsing --
processDocuments :: [String] -> IO [[String]]
processDocuments [] = usage >> exitFailure -- No Tex documents passed
processDocuments documents = mapM processDocument documents

processDocument :: String -> IO [String]
processDocument docPath = do
            let docName = dropExtension docPath
            let folderPath = takeDirectory docPath
            let document = Document docPath docName folderPath
            -- Prints a header to the user indicating processing is staring
            putStrLn $ formatPrints "START" docPath
            -- Changes the current working directory to where the document is
            setCurrentDirectory folderPath
            -- Parses the header of the current file
            jobList <- buildJobList document
            -- Run each job read from the document header
            let curriedPerformJob = performJob document
            jobOutput <- mapM curriedPerformJob jobList
            -- Ensures a pretty newline between each without adding 
            putStrLn ""
            return jobOutput

performJob :: Document -> Job -> IO String
performJob document (Job operation function) = printJob >> function document
    where printJob = putStrLn $ formatPrints "JOB" operation

-- Helper Functions --
usage :: IO ()
usage = putStrLn "usage: aaudoc texfiles"

couldBeTex :: String -> Bool -- Checks if a document could be a Tex documents
couldBeTex docPath = takeExtension docPath `elem` ["", ".", ".tex"]

expandTexPath :: String -> String -- Loosens the requirements of document paths
expandTexPath documentPath = case (last documentPath) of 
                                'x' -> documentPath
                                '.' -> documentPath ++ "tex"
                                _ -> documentPath ++ ".tex"

formatPrints :: String -> String -> String
formatPrints "START" fileName = "-- Processing: " ++ fileName
formatPrints "JOB" operation = "   " ++ operation
