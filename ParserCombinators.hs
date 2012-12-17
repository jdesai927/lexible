{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- CIS 552, University of Pennsylvania
-- based on Parsec and ReadP parsing libraries
module ParserCombinators where

import ParserTrans
import Control.Monad
import Data.Char

-- | Parsers for specific sorts of characters 
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace
   
-- | Generalized char. Parses for a specific value.
token :: Eq b => b -> GenParser b b
token c = satisfy (c ==)

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
-- No real reason to generalize as it doesn't really
-- mean anything to parse a list of tokens into a list
-- of tokens.
string :: String -> Parser String
string = mapM token

-- | given a parser, apply it as many times as possible                         
-- and return the answer in a list
many   :: GenParser b a -> GenParser b [a]
many p = many1 p <|> many0
   where many0 = return []
                    
-- | given a parser, apply it as many times as possible,
-- but at least once.
many1 :: GenParser b a -> GenParser b [a]
many1 p = do x  <- p
             xs <- many p
             return $ x:xs

-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl :: GenParser a b -> GenParser a (b -> b -> b) -> b -> GenParser a b
chainl p op x = chainl1 p op <|> return x

-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: GenParser b a -> GenParser b (a -> a -> a) -> GenParser b a
p `chainl1` pop = p >>= rest
    where rest x = next x <|> return x
          next x = do o <- pop
                      y <- p
                      rest $ x `o` y
                      
                      
-- | Combine all parsers in the list (sequentially)
choice :: [GenParser b a] -> GenParser b a
choice = foldr (<|>) $ fail ""

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between :: GenParser b open -> GenParser b a -> GenParser b close -> GenParser b a
between open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: GenParser b a -> GenParser b sep -> GenParser b [a]
sepBy p sep = sepBy1 p sep <|> return []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: GenParser b a -> GenParser b sep -> GenParser b [a]
sepBy1 p sep = liftM2 (:) p $ many $ sep >> p

-- | Constant parser.
constP :: String -> a -> Parser a
constP s x = liftM (\_ -> x) $ string s

-- | Permissible regular expression characters.
regexChars :: [Char]
regexChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++
  [':', ';', '=', '<', '>', '&', '%', '-', '/', '\\', '~',
  ',', '[', ']', '`', '@', '$', '_', '"', '#', '\'', '!',
  '^']

-- | Characters that cannot, non-escaped, be part of a regular expression.
otherChars :: [Char]
otherChars = ['(', ')', '+', '*', '?', '.', '|']

-- | Basic regular expression character parser.
regexCharP :: Parser Char
regexCharP = choice $ map token regexChars

-- | Generalized between case when the open and close tokens are simple.
makeBetween :: Char -> Char -> Parser a -> Parser a
makeBetween o c p = between (token o) p (token c)

-- | Parser for expression between parens.
parens :: Parser a -> Parser a
parens = makeBetween '(' ')'

-- | Parser for expression between braces.
braces :: Parser a -> Parser a
braces = makeBetween '{' '}'

-- | Parser for expression between square brackets.
brackets :: Parser a -> Parser a
brackets = makeBetween '[' ']'

bsp :: Parser a -> Parser a
bsp p = between (many space) p (many space)