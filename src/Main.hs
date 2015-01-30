-- Project Level Imports --
import File (buildJobList)
import Parsers (addJobOutputParser)
import Datatypes (Job (..), Document (..))

-- Global Level Imports --
import Data.List (nub)
import Paths_AAUDOC (version)
import Control.DeepSeq (($!!))
import Data.Version (showVersion)
import System.Environment (getArgs)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (filterM, (>=>), void)
import Control.Exception (catch, IOException, ErrorCall)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist)
import System.FilePath (takeExtension, dropExtension, takeDirectory, pathSeparator)

-- Main Function --
main :: IO ()
main = getArgs >>= processArguments >>= preparePaths >>= mapM_ processDocument
        where preparePaths = filterM doesFileExist . nub . map expandTexPath . filter couldBeTex

-- Argument Processing
processArguments :: [String] -> IO [String]
processArguments args
            | "--help" `elem` args = printUsage >> exitSuccess
            | "-h" `elem` args = printUsage >> exitSuccess
            | "--version" `elem` args = printVersion >> exitSuccess
            | "-v" `elem` args = printVersion >> exitSuccess
            | null args = printUsage >> exitFailure -- No Tex documents passed
            | otherwise = return args

printUsage :: IO ()
printUsage = putStrLn "usage: aaudoc texfiles"

printVersion :: IO ()
printVersion = putStrLn $ "aaudoc, version " ++ showVersion version

-- Document Processsing --
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
            let document = Document relativeDocPath docName
            -- Parses the header of the current file and builds jobs
            jobList <- catch (buildJobList document) handleFileIOException
            -- Adds output parsers to the jobs that support it to minimise useless output
            let jobListWithParsers = map addJobOutputParser jobList
            -- Forces evaluation to catch any errors in the document header before running any jobs
            safeJobList <- catch (return $!! jobListWithParsers) handleHeaderError
            -- Runs each job created from the header and handles all IO errors thrown by them
            catch (mapM_ (performJob document >=> printJobErrors) safeJobList) handleJOBIOException
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
    where printStreams = printJobOutput stdout >> printJobOutput stderr

-- Error Handling Functions --
handleFileIOException :: IOException -> IO [a]
handleFileIOException ioe = putStrLn ("     ----- IO Error -----\n\
                                \     " ++ show ioe ++ "\n\n\
                                \     AAUDOC was unable to read the document's configuration header,\n\
                                \     please verify that reading is allowed by the document's permissions.\n\
                                \     ----- IO Error -----") >> return []

handleHeaderError :: ErrorCall -> IO [a]
handleHeaderError ec = putStrLn ("     ----- Header Error -----\n\
                                \     " ++ show ec ++ "\n\n\
                                \     AAUDOC found unsupported operations in the configuration header, please verify\n\
                                \     that the document's header does not contain spelling mistakes or misplaced whitespace.\n\
                                \     ----- Header Error -----") >> return []

handleJOBIOException :: IOException -> IO ()
handleJOBIOException ioe = putStrLn $ "     ----- IO Error -----\n\
                                \     " ++ show ioe ++ "\n\n\
                                \     AAUDOC was unable to perform one of the operations specified in the document's\n\
                                \     header, please verify that the necessary folders are readable and writeable, and that\n\
                                \     the output of each operation does not contain characters with invalid character encoding.\n\
                                \     ----- IO Error -----"

-- Helper Functions --
couldBeTex :: String -> Bool -- Checks if a document could be a Tex documents
couldBeTex docPath = takeExtension docPath `elem` ["", ".", ".tex", ".pdf"]

expandTexPath :: String -> String -- HACK: matches last char as all possibilities, as seen above, can be differentiated
expandTexPath documentPath = case last documentPath of
                                'x' -> documentPath
                                '.' -> documentPath ++ "tex"
                                'f' -> dropExtension documentPath ++ ".tex"
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

printJobOutput :: String -> IO ()
printJobOutput "" = return ()
printJobOutput output = putStr (indentJobOutput (errorBorder ++ ('\n' : output) ++ errorBorder)) >> waitForUserInput
    where errorBorder = "-----------------------------------"
          waitForUserInput = putStrLn "     Press Return to to continue, or Ctrl-C to terminate" >> pause

{- The use of getChar is a hack to pause execution, the bogus error handling should
prevent Windows from printing stdin closed errors when Ctrl-C is used to terminate the program
-}
pause :: IO ()
pause = void (catch getChar ignoreErrors)
    where ignoreErrors :: IOException -> IO Char
          ignoreErrors _ = return '\n'

indentJobOutput :: String -> String
indentJobOutput output = unlines $ map ("     " ++) $ lines output
