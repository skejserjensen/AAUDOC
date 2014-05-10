module Datatypes
( Document (..)
, Job (..)
) where

-- Global Level Imports --
import System.Exit (ExitCode (..), ExitCode)

{- Represent a LaTeX document being compiled
    path: the of the document with suffix
    name: the of the document without suffix
    folderPath: the path of the folder the document resides in
-}
data Document = Document { path :: String
                         , name :: String
                         , folderPath :: String
                         } deriving Show

{- Something that should be done to a document
    operation: a printable string documenting the job
    function: a function taking a document of performing the job
-}
data Job = Job { operation :: String
               , function :: Document -> IO (ExitCode, String, String)
               }
