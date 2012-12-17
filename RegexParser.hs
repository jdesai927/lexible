module RegexParser (regexP) where
import Prelude
import Test.HUnit
import Test.QuickCheck
import Regex
import Data.Char(isAlpha)
import Control.Monad(liftM)
import ParserTrans
import ParserCombinators

-- | Parser for all permissible regexes.
regexP :: Parser Regex
regexP = choice [unionP, seqP, parens regexP, starP, plusP, 
                 questionP, classP, escapeP, dotP, symP]

-- | Test helper, takes a specific regex parser and a string
--   and returns the string parsed by the given parser.
--   Returns epsilon if false since epsilon cannot be parsed.
genRes :: Parser Regex -> String -> Regex
genRes p s = let rg = doParse p s in
             case rg of
               []   -> Epsilon
               x:_  -> fst x



-- | Parser for character regexes.
symP :: Parser Regex
symP = liftM Symbol regexCharP

-- | Character regex parser tests.

ts1 :: Test
ts1 = genRes symP "c" ~=? Symbol 'c'

-- | '.' is a reserved character, so this should fail.
ts2 :: Test
ts2 = genRes symP "." ~=? Epsilon

ts3 :: Test
ts3 = genRes symP "3" ~=? Symbol '3'



-- | Parser for escaped characters.
escapeP :: Parser Regex
escapeP = token '\\' >> liftM Symbol getC

-- | Escape character parser tests.

tesc1 :: Test
tesc1 = genRes escapeP "\\." ~=? Symbol '.'

-- | Nothing after escape sequence.
tesc2 :: Test
tesc2 = genRes escapeP "\\" ~=? Epsilon

tesc3 :: Test
tesc3 = genRes escapeP "\\\\" ~=? Symbol '\\'



-- | Parser for regexes that may be modified by a subsequent operator.
opP :: Char -> (Regex -> Regex) -> Parser Regex
opP c f = do rg <- choice [parens regexP, classP, escapeP, dotP, symP]
             token c
             return $ f rg



-- | Parser for the Kleene star operator.
starP :: Parser Regex
starP = opP '*' Star

-- Tests for the star parser are in the general test section.



-- | Parser for the plus regular operator.
plusP :: Parser Regex
plusP = opP '+' $ \rg -> Concat rg $ Star rg

-- Tests for the plus parser are in the general test section.



-- | Parser for the question mark regular operator.
questionP :: Parser Regex
questionP = opP '?' $ Union Epsilon

-- | Tests for the question parser.

tq1 :: Test
tq1 = genRes questionP "x?" ~=? Union (Symbol 'x') Epsilon

tq2 :: Test
tq2 = genRes questionP "(abc)?" ~=? 
  Union (Concat (Symbol 'a') (Concat (Symbol 'b') (Symbol 'c'))) Epsilon

-- | Just a question mark is meaningless.
tq3 :: Test
tq3 = genRes questionP "?" ~=? Epsilon



-- | Generic character class helper that contructs the regex for the
--   character class by enumerating the list defined by the class.
classPH :: Parser Char -> Parser Regex
classPH p = do a <- p
               token '-'
               b <- p
               let l = map Symbol [a..b] in 
                 return $ foldr Union (Symbol b) $ init l

-- | Parser for character class regexes.
classP :: Parser Regex
classP = brackets (classPH alpha <|> classPH digit)

-- Tests for the character class parser are in the general test section.

-- | Parser for the dot regex.
dotP :: Parser Regex
dotP = constP "." $ foldr Union (Symbol '9') $ map Symbol cs
  where cs = regexChars ++ otherChars

-- | The regex ".*" should match any string.
prop_dot :: String -> Bool
prop_dot s = match (regToRI $ result ".*") $ 
               filter (\c -> elem c $ regexChars ++ otherChars) s

-- | Tests for the below parsers are all below.

-- | The main list of what to look for next when the parser is
--   in a sequence or union.
priorityList :: Parser Regex
priorityList = choice [starP, plusP, questionP, parens regexP, classP, 
                       escapeP, dotP, symP]

-- | Parser for sequences.
seqP :: Parser Regex
seqP = liftM (foldr Concat Epsilon) $ many1 priorityList

-- | Parser for unions.
unionP :: Parser Regex
unionP = chainl1 (seqP <|> priorityList) $ constP "|" Union

-- | Test helper, parses a regex from a string and returns the result.
result :: String -> Regex
result = genRes regexP

-- | Tests for more complex regexes. 
 
simple_seq :: Regex 
simple_seq = result "abc" 
 
simple_union :: Regex 
simple_union = result "a|b" 
 
simple_star :: Regex 
simple_star = result "a*" 
 
un_seq_1 :: Regex 
un_seq_1 = result "a(b|a)b(b|c)" 
 
complex :: Regex 
complex = result "a*b*|(c|d)*e*" 
 
char_class_1 :: Regex 
char_class_1 = result "[a-z]*" 
 
char_class_2 :: Regex 
char_class_2 = result "[a-w]*" 
 
compound1 :: Regex 
compound1 = result "[a-k]z(a|b)*" 
 
compound2 :: Regex 
compound2 = result "[a-g].(x|y|z)*t?" 
 
compound3 :: Regex 
compound3 = result "[a-c]+[q-t]*" 
 
tseq1 :: Test 
tseq1 = match (regToRI simple_seq) "abc" ~=? True 
 
tunion1 :: Test 
tunion1 = match (regToRI simple_union) "a" ~=? True 
 
tunion2 :: Test 
tunion2 = match (regToRI simple_union) "b" ~=? True 
 
tunion3 :: Test 
tunion3 = match (regToRI simple_union) "" ~=? False 
 
tunion4 :: Test 
tunion4 = match (regToRI simple_union) "c" ~=? False 
 
tstar1 :: Test 
tstar1 = match (regToRI simple_star) "a" ~=? True 
 
tstar2 :: Test 
tstar2 = match (regToRI simple_star) "aaaaaaaa" ~=? True 
 
tstar3 :: Test 
tstar3 = match (regToRI simple_star) "" ~=? True 
 
tstar4 :: Test 
tstar4 = match (regToRI simple_star) "aaaaab" ~=? False 
 
tstar5 :: Test 
tstar5 = match (regToRI simple_star) "aaaaabaaaa" ~=? False 
 
thard1 :: Test 
thard1 = match (regToRI un_seq_1) "abbb" ~=? True 
 
thard2 :: Test 
thard2 = match (regToRI un_seq_1) "aabc" ~=? True 
 
thard3 :: Test 
thard3 = match (regToRI un_seq_1) "abbc" ~=? True 
 
thard4 :: Test 
thard4 = match (regToRI un_seq_1) "abcb" ~=? False 
 
tcomplex1 :: Test 
tcomplex1 = match (regToRI complex) "" ~=? True 
 
tcharclass1 :: Test 
tcharclass1 = match (regToRI char_class_1) "" ~=? True 
 
tcharclass2 :: Test 
tcharclass2 = match (regToRI char_class_1) "abcdefghijk" ~=? True 
 
tcharclass3 :: Test 
tcharclass3 = match (regToRI char_class_1) "abcdefghijk%" ~=? False 
 
tcharclass4 :: Test 
tcharclass4 = match (regToRI char_class_1) "abcdefMghijk" ~=? False 
 
tcharclass5 :: Test 
tcharclass5 = match (regToRI char_class_2) "abcdzefghijk" ~=? False 
 
tcompound1 :: Test 
tcompound1 = match (regToRI compound1) "jza" ~=? True 
 
tcompound2 :: Test 
tcompound2 = match (regToRI compound1) "az" ~=? True 
 
tcompound3 :: Test 
tcompound3 = match (regToRI compound1) "baz" ~=? False 
 
tcompound4 :: Test 
tcompound4 = match (regToRI compound1) "jzabbbaaa" ~=? True 
 
tcompound5 :: Test 
tcompound5 = match (regToRI compound1) "aaa" ~=? False 
 
tcompound6 :: Test 
tcompound6 = match (regToRI compound2) "bd" ~=? True 
 
tcompound7 :: Test 
tcompound7 = match (regToRI compound2) "a9zy" ~=? True 
 
tcompound8 :: Test 
tcompound8 = match (regToRI compound2) "gDt" ~=? True 
 
tcompound9 :: Test 
tcompound9 = match (regToRI compound2) "aatt" ~=? False 
 
tcompound10 :: Test 
tcompound10 = match (regToRI compound2) "fog" ~=? False 
 
tcompound11 :: Test 
tcompound11 = match (regToRI compound3) "a" ~=? True 
 
tcompound12 :: Test 
tcompound12 = match (regToRI compound3) "abc" ~=? True 
 
tcompound13 :: Test 
tcompound13 = match (regToRI compound3) "" ~=? False 
 
tcompound14 :: Test 
tcompound14 = match (regToRI compound3) "acbqstr" ~=? True 
 
tcompound15 :: Test 
tcompound15 = match (regToRI compound3) "qrs" ~=? False 
 
allTests :: Test 
allTests = TestList [ts1, ts2, ts3, tesc1, tesc2, tesc3, tseq1, tq3,
                     tunion1, tunion2, tunion3, tunion4, tq1, tq2,
                     tstar1, tstar2, tstar3, tstar4, tstar5, thard1, 
                     thard2, thard3, thard4, tcomplex1, tcharclass1,  
                     tcharclass2, tcharclass3, tcharclass4, tcharclass5, 
                     tcompound1, tcompound2, tcompound3, tcompound4,  
                     tcompound5, tcompound6, tcompound7, tcompound8, 
                     tcompound9, tcompound10, tcompound11, tcompound12, 
                     tcompound13, tcompound14, tcompound15] 

main :: IO ()
main = do _ <- runTestTT allTests
          quickCheck prop_dot
          return ()