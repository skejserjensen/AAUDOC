module Datatypes
( Document (..)
, Job (..)
) where

-- Global Level Imports --
import System.Exit (ExitCode (..), ExitCode)
import Control.DeepSeq

{- Represent a LaTeX document being compiled
    path: the of the document with suffix
    name: the of the document without suffix
    folderPath: the path of the folder the document resides in
-}
data Document = Document { path :: String
                         , name :: String
                         , folderPath :: String
                         }

 -- Something that should be done to a document

data Job =
    {- operation: a printable string for displaying when running the job
       function: a function taking a document and performing the job
    -} StandardJob String (Document -> IO (ExitCode, String, String))

    {- command: a string containing the name of the command to be executed
       operation: a printable string for displaying when running the job
       function: a function taking a document and performing the job
    -} | CommandJob String String (Document -> IO (ExitCode, String, String))

instance NFData Job where
    rnf (StandardJob operation function) = operation `seq` function `seq` ()
    rnf (CommandJob command operation function) = command `seq` operation `seq` function `seq` ()
