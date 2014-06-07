module Macros
( expandMacros
) where

-- Project Level Imports --
import Datatypes (Document (..))

-- Public Functions --
expandMacros :: Document -> [String] -> [String]
expandMacros doc = foldl (expandToFull doc) []
    where expandToFull document acc line = acc ++ expandMacro document line

expandMacro :: Document -> String -> [String]
expandMacro _ "%macro compile" = macroCompile
expandMacro _ "%macro compile-with-bib" = macroCompileWithBibTex
expandMacro _ "%macro compile-with-index" = macroCompileWithIndex
expandMacro doc "%macro compile-doc" = macroCompileDoc $ name doc
expandMacro _ ('%' : 'm' : 'a' : 'c' : 'r' : 'o' : ' ' : macro) =
        error $ "ExpandMacro: unknown macro definition in header " ++ show macro
expandMacro _ headerLine = [headerLine]

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
