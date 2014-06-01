module Datatypes
( Document (..)
, Job (..)
) where

-- Global Level Imports --
import Control.DeepSeq
import System.Exit (ExitCode (..), ExitCode)

{- Represent a LaTeX document being compiled
    path: the of the document with suffix
    name: the of the document without suffix
-}
data Document = Document { path :: String
                         , name :: String
                         }

 -- A Job is something that should be done to a document
data Job =
    {- operation: a printable string for displaying when running the job
       function: a function taking a document and performing the job
    -} StandardJob String (Document -> IO (ExitCode, String, String))

    {- command: a string containing the name of the command to be executed
       operation: a printable string for displaying when running the job
       function: a function taking a document and performing the job
    -} | CommandJob String String (Document -> IO (ExitCode, String, String))

{- Job is made an instance of NFData to support the use of `$!!`, forcing the documents
   header to be fully parsed and checked for errors before any of the jobs are executed
-}
instance NFData Job where
    rnf (StandardJob operation function) = operation `seq` function `seq` ()
    rnf (CommandJob command operation function) = command `seq` operation `seq` function `seq` ()
