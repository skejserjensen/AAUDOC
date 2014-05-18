-- Project Level Imports --
import File (buildJobList)
import Parsers (addJobOutputParser)
import Datatypes (Job (..), Document (..))

-- Global Level Imports --
import Control.DeepSeq (($!!))
import System.Environment (getArgs)
import Control.Monad (filterM, (>=>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (ExitCode (..), exitFailure)
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist)
import System.FilePath (takeExtension, dropExtension, takeDirectory, pathSeparator)

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
            -- Prints a header to the user indicating processing is staring
            putStrLn $ formatPrints "BEGIN" docPath
            startTimeStamp <- getPOSIXTime
            -- Changes the current working directory to where the document is
            oldCurrentDirectory <- getCurrentDirectory
            setCurrentDirectory $ takeDirectory docPath
            -- Strip the path so relative folder paths and file names can be used
            let relativeDocPath = reverse $ takeWhile (/= pathSeparator) $ reverse docPath
            let docName = dropExtension relativeDocPath
            let docFolder = takeDirectory relativeDocPath
            let document = Document relativeDocPath docName docFolder
            -- Parses the header of the current file and builds jobs
            jobList <- buildJobList document
            -- Adds output parsers to the jobs that support it to minimise useless output
            let jobListWithParsers = map addJobOutputParser jobList
            -- Run each job read from the document header
            mapM_ (performJob document >=> printJobErrors) $!! jobListWithParsers
            -- We restore the old working directory so no changes are done to the environment
            setCurrentDirectory oldCurrentDirectory
            -- Outputs the complete compile time for the documnent at the end
            endTimeStamp <- getPOSIXTime
            putStrLn $ formatPrints "END" $ show $ endTimeStamp - startTimeStamp
            return ()

performJob :: Document -> Job -> IO (ExitCode, String, String)
performJob document (StandardJob operation function) = printJob operation >> function document
performJob document (CommandJob _ operation function) = printJob operation >> function document

printJobErrors :: (ExitCode, String, String) -> IO ()
printJobErrors (ExitSuccess, _, _) = return ()
printJobErrors (ExitFailure exitCode, stdout, stderr) = printErrorCode exitCode >> printStreams
    where printStreams = printError stdout >> printError stderr

-- Helper Functions --
usage :: IO ()
usage = putStrLn "usage: aaudoc texfiles"

couldBeTex :: String -> Bool -- Checks if a document could be a Tex documents
couldBeTex docPath = takeExtension docPath `elem` ["", ".", ".tex"]

expandTexPath :: String -> String -- Loosens the requirements of document paths
expandTexPath documentPath = case last documentPath of
                                'x' -> documentPath
                                '.' -> documentPath ++ "tex"
                                _ -> documentPath ++ ".tex"

printJob :: String -> IO ()
printJob operation = putStrLn $ formatPrints "JOB" operation

formatPrints :: String -> String -> String
formatPrints "BEGIN" fileName = "-- Processing: " ++ fileName
formatPrints "JOB" printOperation = "   " ++ printOperation
formatPrints "ERROR" exitCode = "     Job terminated with error code: " ++ exitCode
formatPrints "END" compileTime = "   [Time elapsed]: " ++ compileTime ++ "\n"
formatPrints _ input = error $ "FormatPrints: unknown argument to print formatter \"" ++ input ++ "\""

printErrorCode :: Int -> IO ()
printErrorCode exitCode = putStrLn (formatPrints "ERROR" $ show exitCode)

printError :: String -> IO ()
printError "" = return ()
printError output = putStr (indentJobOutput (errorBorder ++ ('\n' : output) ++ errorBorder)) >> waitForUserInput
    where errorBorder = "-----------------------------------" -- getChar is a hack to pause exection
          waitForUserInput = putStrLn "     Press Return to to continue, or Ctrl-C to terminate" >> getChar >> return ()

indentJobOutput :: String -> String
indentJobOutput output = unlines $ map ("     " ++) $ lines output
