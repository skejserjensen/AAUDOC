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
expandMacro doc "%macro compile-doc" = macroCompileDoc $ name doc
expandMacro _ "%macro compile-with-index" = macroCompileWithIndex 
expandMacro _ "%macro compile-with-bib" = macroCompileWithBibTex
expandMacro _ ('%' : 'm' :'a' : 'c' : 'r' : 'o' : ' ' : macro) = 
        error $ "ExpandMacro: unknown macro definition in header " ++ show macro
expandMacro _ headerLine = [headerLine]

macroCompileDoc :: String -> [String]
macroCompileDoc docName = ["%link Documents/" ++ docName ++ "/ Documents/" ++
        docName ++ "/index.tex", "%command lualatex", "%command bibtex",
        "%command lualatex", "%command lualatex", "%clean"]

macroCompileWithIndex :: [String]
macroCompileWithIndex = ["%link Documents/ Documents/index.tex", "%command lualatex",
        "%command makeindex", "%command lualatex", "%command lualatex", "%clean"]

macroCompileWithBibTex :: [String]
macroCompileWithBibTex = ["%link Documents/ Documents/index.tex", "%command lualatex",
        "%command bibtex", "%command lualatex", "%command lualatex", "%clean"]
