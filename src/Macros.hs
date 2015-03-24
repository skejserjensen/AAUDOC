module Macros
( expandMacros
) where

-- Global Level Imports --
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Control.DeepSeq (($!!))
import qualified Data.Map as Map

-- Project Level Imports --
import Datatypes (Document (..))

-- Public Functions --
expandMacros :: Document -> [String] -> [String]
expandMacros doc = foldl (expandToFull doc) []
    where expandToFull document acc line = acc ++ expandMacro document line

expandMacro :: Document -> String -> [String]
expandMacro doc line
  | "%macro compile-with-index" `isPrefixOf` line =
        attachArguments macroCompileWithIndex args
  | "%macro compile-with-bib" `isPrefixOf` line =
      attachArguments macroCompileWithBibTex args
  | "%macro compile-doc" `isPrefixOf` line =
      attachArguments (macroCompileDoc $ name doc) args
  | "%macro compile" `isPrefixOf` line =
      attachArguments macroCompile args
  | "%macro aaudoc" `isPrefixOf` line =
      attachArguments (macroAAUDoc $ name doc) args
  | "%macro" `isPrefixOf` line =
      error $ "ExpandMacro: unknown macro definition in header " ++ dropWhile (/= ' ') line
  | otherwise = [line]
    where args = unwords $ tail $ tail $ words line -- Drops %macro and name

-- Macro Functions --
macroCompile :: [String]
macroCompile = ["%link Documents/ index.tex", "%command lualatex", "%command lualatex", "%clean"]

macroCompileWithBibTex :: [String]
macroCompileWithBibTex = ["%link Documents/ index.tex", "%command lualatex",
                          "%command bibtex", "%command lualatex", "%command lualatex",
                          "%clean"]

macroCompileWithIndex :: [String]
macroCompileWithIndex = ["%link Documents/ index.tex", "%command lualatex",
                         "%command makeindex", "%command lualatex", "%command lualatex",
                         "%clean"]

macroCompileDoc :: String -> [String]
macroCompileDoc docName = ["%link-doc Documents/" ++ docName ++ "/ Documents/" ++ docName ++ "/index.tex",
                           "%command lualatex", "%command bibtex", "%command lualatex", "%command lualatex",
                           "%clean"]

macroAAUDoc :: String -> [String]
macroAAUDoc docName = ["%link Documents/" ++ docName ++ "/ Documents/index.tex", "%command lualatex",
                       "%command bibtex", "%command lualatex", "%command lualatex", "%clean"]

-- Helper Functions --
attachArguments :: [String] -> String -> [String]
attachArguments macro args = zipWith (\ item index -> item ++ getArgs argsMap item (show (index :: Integer))) macro [1 .. ]
  where argsMap = convertToMap $!! distributeArguments $ map convertToTuple $ splitMacroArguments args []
        convertToMap list = checkArgs macro $ Map.fromListWith (\ itemOne itemTwo -> itemOne ++ " " ++ itemTwo) list
        convertToTuple arg = (takeWhile (/= '=') arg, safeTail $ dropWhile (/= '=') arg)
        safeTail arg = if null arg
                          then error "Malformed macro arguments, expected seperation of placement and arguments using \"=\""
                          else tail arg

getArgs :: Map.Map String String -> String -> String -> String
getArgs argsMap ('%' : 'c' : 'o' : 'm' : 'm' : 'a' : 'n' : 'd' : ' ' : op) index =
    " " ++ Map.findWithDefault "" op argsMap ++ " " ++ Map.findWithDefault "" index argsMap
getArgs argsMap _ index = " " ++ Map.findWithDefault "" index argsMap

checkArgs :: [String] -> Map.Map String String -> Map.Map String String
checkArgs macro argsMap = if Map.null checkedMap
                             then argsMap
                             else error $ "CheckArgs: unsed macro arguments in header " ++ show checkedMap
  where checkedMap = fst $ foldl (\ (am, index) item -> updateAcc am item index) (argsMap, 1) macro
        updateAcc am item index = (deleteArg am (dropWhile (/= ' ') item) index, index + 1)
        deleteArg am item index = Map.delete (show (index :: Integer)) $ Map.delete (dropWhile isSpace item) am

distributeArguments :: [(String, String)] -> [(String, String)]
distributeArguments = foldl addArguments []
  where addArguments acc (op, arg) = map (\ item -> (item, arg)) (splitOnComma op) ++ acc
        splitOnComma op = words $ map (\ char -> if char == ',' then ' ' else char) op

splitMacroArguments :: String -> [String] -> [String]
splitMacroArguments "" acc = reverse acc
splitMacroArguments ('-' : args) acc = splitMacroArguments rest (arg : acc)
  where (arg, rest) = nextArgument args ' ' ""
splitMacroArguments _ _ = error "Malformed macro arguments, expected first char to be \"-\""

nextArgument :: String -> Char -> String -> (String, String)
nextArgument [] _ acc = (reverse acc, "")
nextArgument (' ' : xs) ' ' acc = (reverse acc, dropWhile isSpace xs)
nextArgument ('\'' : xs) ' ' acc = nextArgument xs '\'' acc
nextArgument ('\'' : xs) '\'' acc = (reverse acc, dropWhile isSpace xs)
nextArgument ('"' : xs) ' ' acc = nextArgument xs '"' acc
nextArgument ('"' : xs) '"' acc = (reverse acc, dropWhile isSpace xs)
nextArgument (x : xs) qouted acc = nextArgument xs qouted $ x : acc

