module Main where
import Regex
import RegexParser
import ParserTrans
import ParserCombinators
import System.Environment(getArgs)
import Control.Monad(liftM, liftM2)
import Data.List(elemIndex, intercalate)

-- | See the README for more specific information on the output file
--   and what each part of it is used for.

-- | An "Action" is a string representing Haskell source. This abstraction
--   is to distinguish strings representing parsed Haskell source from
--   strings representing generated Haskell source.
type Action = String

-- | Parser that gives back an action.
actionP :: Parser Action
actionP = bsp $ braces $ many1 $ choice $ regexCharP:space:map token otherChars

-- | Parses all regexes and their respective actions from the file.
tokenP :: Parser String
tokenP = bsp $ liftM buildMap $ sepBy1 (liftM2 (,) fRegP actionP) $ many1 space
  where fRegP =  token '|' >> (bsp $ liftM regToRI regexP)

-- | Creates a string that represent a list of the user's functions
--   and the individual definitions of each of those functions.
buildActions :: [Action] -> String
buildActions as =
  -- list of function names, ["fun0", "fun1", ...]
  let funs = map ((++) "fun" . show) [0..length as - 1] in
  "\nallFuns :: [String -> Token]\nallFuns = [" ++ -- function list header
  intercalate ", " funs ++ "]\n"                ++ -- function name list
  (foldr buildFunc "" $ zip funs as)               -- function definitions
  where buildFunc (a, s) c = "\n" ++ a ++ " :: String -> Token\n" ++
                              a ++ " = " ++ s ++ "\n" ++ c

-- | Takes the results of the first function, builds a string that
--   represents a map from regexes to functions as per the user's
--   specification, and concatenates it with the string created
--   in buildActions.
buildMap :: [(RegInfo, Action)] -> String
buildMap conf = let (rs,as) = unzip conf in
  "\nactions :: [(RegInfo, String -> Token)]\nactions = zip " ++
  show rs ++ " allFuns\n" ++ buildActions as

-- | Parses the configuration file into a string representing
--   the generated lexer source code.
lexFileP :: Parser String
lexFileP = do str "import:"
              s  <- actionP <|> (braces $ str "") -- import can be empty
              str "tokens:"
              rs <- tokenP -- must have at least one token
              str "tokentype:"
              t  <- actionP -- token typedef must be nonempty
              str "code:"
              m  <- actionP -- same with main
              return (buildHeader  ++ -- header
                      s            ++ -- user imports
                      "\n" ++ rs   ++ -- user functions and regex mapping
                      buildMatch   ++ -- matchToken
                      buildScanner ++ -- getAllTokens
                      "\n\n" ++ t  ++ -- Token type definition
                      "\n\n" ++ m)    -- main method and misc. code

  where act = bsp actionP
        str = bsp . string

        -- | Builds string representing the getAllTokens function (see readme).
        buildScanner = 
          "\ngetAllTokens :: String -> [Token]\ngetAllTokens s = "
          ++ "let res = doParse (sepBy1 (many1 $ satisfy $ not . isSpace)"
          ++ " (many1 space)) s in catMaybes $ map matchToken $ if null "
          ++ "res then [] else fst $ head $ res\n"

        -- | Builds string representing the matchToken function (see readme).
        buildMatch = 
          "\nmatchToken :: String -> Maybe Token\nmatchToken s = "
          ++ "maybe Nothing (\\x -> Just $ snd x $ s) $ "
          ++ "find (flip match s . fst) actions\n"

        -- | Constructs a string that represents the header of the output
        --   source code, with information such as the module name and the
        --   imported packages.
        buildHeader = 
          "module Main where\nimport Prelude\nimport Data.List(find)\n"
          ++ "import Data.Char(isSpace)\nimport Data.Maybe(catMaybes)\n"
          ++ "import Regex\nimport ParserTrans\nimport ParserCombinators\n"

-- | Read the file specified from the command line (argument 1),
--   parse it, construct the output file string, and write it to the
--   specified output file (argument 2).
main :: IO ()
main = do as <- getArgs
          s  <- if length as < 2 then error "Error: insufficient arguments."
                  else readFile $ head as
          case doParse lexFileP s of
            []        -> putStr "Error: invalid input. Lexer not generated."
            (st, _):_ -> writeFile (head $ tail as) st