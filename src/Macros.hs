module Macros
( expandMacros
) where

-- Project Level Imports --
import Datatypes (Document (..))

-- Public Functions --
expandMacros :: Document -> [String] -> [String]
expandMacros doc = foldl (expandToFull doc) []
    where expandToFull document acc line = acc ++ expandMacro document (words line)

expandMacro :: Document -> [String] -> [String]
expandMacro _ ("%macro" : "compile" : args) =
        attachArguments macroCompile args
expandMacro _ ("%macro" : "compile-with-bib" : args) =
        attachArguments macroCompileWithBibTex args
expandMacro _ ("%macro" : "compile-with-index" : args) =
        attachArguments macroCompileWithIndex args
expandMacro doc ("%macro" : "compile-doc" : args) =
        attachArguments (macroCompileDoc $ name doc) args
expandMacro _ ("'%macro" : macro) =
        error $ "ExpandMacro: unknown macro definition in header " ++ show macro
expandMacro _ headerLine = headerLine

-- Macro Functions --
macroCompile :: [String]
macroCompile = ["%link Documents/ index.tex", "%command lualatex",
        "%command lualatex", "%clean"]

macroCompileWithBibTex :: [String]
macroCompileWithBibTex = ["%link Documents/ index.tex", "%command lualatex",
        "%command bibtex", "%command lualatex", "%command lualatex", "%clean"]

macroCompileWithIndex :: [String]
macroCompileWithIndex = ["%link Documents/ index.tex", "%command lualatex",
        "%command makeindex", "%command lualatex", "%command lualatex", "%clean"]

macroCompileDoc :: String -> [String]
macroCompileDoc docName = ["%link-doc Documents/" ++ docName ++ "/ Documents/" ++
        docName ++ "/index.tex", "%command lualatex", "%command bibtex",
        "%command lualatex", "%command lualatex", "%clean"]

-- Helper Functions --
attachArguments :: [String] -> [String] -> [String]
attachArguments expandedMacro [] = expandedMacro
attachArguments expandedMacro arguments = foldl attachArgument expandedMacro arguments

attachArgument :: [String] -> String -> [String]
attachArgument expandedMacro ('-' : indexChar : '=' : arg) =
    -- Appends the argument to the job at the specified index
    take index expandedMacro ++ [headerLine ++ (' ' : arg)] ++ drop (index + 1) expandedMacro
    where headerLine = expandedMacro !! index
          index = read [indexChar] - 1
attachArgument _ arg = error $ "AttachArgumetn: invalid macro argument syntax in header " ++ show arg

