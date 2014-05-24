module Jobs
( buildJob
) where

-- Project Level Imports --
import Datatypes (Document (..), Job (..))

-- Project Level Imports --
import Control.Monad (forM)
import Data.List (delete, sort)
import System.Exit (ExitCode (..))
import Control.Exception (throwIO, catch)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode)
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory (doesDirectoryExist, getDirectoryContents, removeFile)

-- Job Creation --
buildJob :: [String] -> Job
buildJob ("%command" : command : arguments) = CommandJob command ("Command: " ++ command)
                                                (commandJob command arguments)
buildJob ("%link" : inputPath : outputPath : []) = StandardJob ("Linkning: " ++ inputPath ++ " => " ++ outputPath)
                                                (linkJob inputPath outputPath)
buildJob ("%clean" : suffixList) = StandardJob ("Cleaning: " ++ show suffixes) (cleanJob suffixes)
    where suffixes = if null suffixList then defaultSuffixList else suffixList
          defaultSuffixList = ["aux", "bbl", "blg", "brf", "ilg", "ind", "idx", "log", "out", "toc"]
buildJob list = error $ "BuildJob: unknown operation in header \"" ++ show list ++ "\""

-- Job Functions --
linkJob :: String -> String -> Document -> IO (ExitCode, String, String)
linkJob inputPath outputPath _ = do
                    -- Searches the specified folder for tex files and a bibliography
                    textFiles <- getFilesWithSuffixRecursive inputPath [".bib", ".tex"]
                    let textFilesWithoutOutputFile = delete outputPath textFiles
                    let uniformOutputLines = map replaceWindowsPathSeparators textFilesWithoutOutputFile
                    let outputFileLines = map formatIndexLines uniformOutputLines
                    -- Prepares the output file and a curried append function with its name
                    let outputAppend = appendFile outputPath
                    writeFile outputPath "%Index automatically generated by AAUDOC-Haskell, \
                            \changes done to the file will be overwritten upon next compilation"
                    outputAppend "\n\\begin{document}"
                    mapM_ (\ line -> outputAppend $ '\n' : line) outputFileLines
                    outputAppend "\n\\end{document}"
                    -- The return of (ExitSuccess, "", "") is added for a uniform interface between all jobs
                    return (ExitSuccess, "", "")

commandJob :: String -> [String] -> Document -> IO (ExitCode, String, String)
commandJob command arguments doc = readProcessWithExitCode command (arguments ++ [name doc]) []

cleanJob :: [String] -> Document -> IO (ExitCode, String, String)
-- The return of (ExitSuccess, "", "") is added for a uniform interface between all jobs
cleanJob suffixToDelete doc = mapM_ prependPath suffixToDelete >> return (ExitSuccess, "", "")
    where prependPath suffix = removeFileIfExists $ name doc ++ "." ++ suffix

-- Helper Functions --
getFilesWithSuffixRecursive :: String -> [String] -> IO [String]
getFilesWithSuffixRecursive directoryPath suffix = files >>= filterWithSuffix
    where files = getContentsRecursive directoryPath
          filterWithSuffix = return . filter (\ file -> takeExtension file `elem` suffix)

-- Published in Real World Haskell Chapter 9 under the (CC BY-NC 3.0) license
getContentsRecursive :: String -> IO [String]
getContentsRecursive topdir = do
  directoryContents <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) directoryContents
  paths <- forM properNames $ \ directoryElement -> do
    let elementPath = topdir </> directoryElement
    isDirectory <- doesDirectoryExist elementPath
    if isDirectory
      then getContentsRecursive elementPath
      else return [elementPath]
  return $ sort $ concat paths

replaceWindowsPathSeparators :: String -> String
replaceWindowsPathSeparators line = reverse $ foldl replaceSeperator "" line
    where replaceSeperator acc char = if char == '\\' then '/' : acc else char : acc

formatIndexLines :: String -> String
formatIndexLines line
    | takeExtension line == ".tex" = "\\input{" ++ dropExtension line ++ "}"
    | takeExtension line == ".bib" = "\\bibliography{" ++ dropExtension line ++ "}"
    | otherwise = error $ "LinkJob: unknown filetype passed to index fomatter \"" ++ line ++ "\""

removeFileIfExists :: String -> IO ()
removeFileIfExists filePath = removeFile filePath `catch` handleFileNotExists
    where handleFileNotExists exeception
            | isDoesNotExistError exeception = return ()
            | otherwise = throwIO exeception
