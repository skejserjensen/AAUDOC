import System.Environment (getArgs)
import Control.Monad (filterM, (>=>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (ExitCode(..), exitFailure)
import System.Directory (setCurrentDirectory, doesFileExist)
import System.FilePath (takeExtension, dropExtension, takeDirectory)

import File

-- Main Function --
main :: IO ()
main = getArgs >>= preparePaths >>= processDocuments >> return ()
        where preparePaths = filterM doesFileExist . map expandTexPath . filter couldBeTex

-- Document Processsing --
processDocuments :: [String] -> IO ()
processDocuments [] = usage >> exitFailure -- No Tex documents passed
processDocuments documents = mapM_ processDocument documents

processDocument :: String -> IO ()
processDocument docPath = do
            let docName = dropExtension docPath
            let folderPath = takeDirectory docPath
            let document = Document docPath docName folderPath
            -- Prints a header to the user indicating processing is staring
            putStrLn $ formatPrints "BEGIN" docPath
            startTimeStamp <- getPOSIXTime
            -- Changes the current working directory to where the document is
            setCurrentDirectory folderPath
            -- Parses the header of the current file
            jobList <- buildJobList document
            -- Run each job read from the document header
            let performJobAndPrintErrors = performJob document >=> printJobErrors 
            jobOutput <- mapM performJobAndPrintErrors jobList
            -- Outputs the complete compile time for the documnent at the end
            endTimeStamp <- getPOSIXTime
            putStrLn $ formatPrints "END" $ show $ endTimeStamp - startTimeStamp
            return () 

performJob :: Document -> Job -> IO (ExitCode, String, String)
performJob document (Job operation function) = printJob >> function document
    where printJob = putStrLn $ formatPrints "JOB" operation

printJobErrors :: (ExitCode, String, String) -> IO ()
printJobErrors (ExitSuccess, _, _) = return () -- getChar is a hack to pause exection
printJobErrors (ExitFailure exitCode, stdout, stderr) = printError >> getChar >> return ()
    where printError = putStrLn (formatPrints "ERROR" $ show exitCode) >> putStr stdout 

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
formatPrints "BEGIN" fileName = "-- Processing: " ++ fileName
formatPrints "JOB" operation = "   " ++ operation
formatPrints "ERROR" exitCode = "     Job terminated with error code: " ++ exitCode
formatPrints "END" compileTime = "   [Time elapsed]: " ++ compileTime ++ "\n"
