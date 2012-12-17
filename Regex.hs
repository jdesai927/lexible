module Regex where
import Prelude
import Test.HUnit

-- | The Info type represents the state of a Glushkov automaton. If isFinal
--   is True, the automaton is in an accept state. If isEmpty is true, it
--   means that the automaton accepts the empty string, and so its accept
--   state can be assumed to be true if the input is any empty string.
data Info = Info { isFinal :: Bool, isEmpty :: Bool } deriving (Show, Eq)

-- | The regex type is the type returned by the regular expression parser.
--   This type represents a regular expression, from which a Glushkov automaton
--   accepting the language of that regular expression can be constructed.
data Regex =  Symbol Char
            | Union Regex Regex
            | Concat Regex Regex
            | Star Regex
            | Epsilon
      deriving (Show)

instance Eq Regex where
  Symbol x          == Symbol y           = x == y
  Union r1 r2       == Union r3 r4        = (r1 == r3 && r2 == r4) ||
                                             (r1 == r4 && r2 == r3)
  rg                == Concat rg' Epsilon = rg' == rg
  Concat rg Epsilon == rg'                = rg == rg'
  Concat r1 r2      == Concat r3 r4       = r1 == r3 && r2 == r4
  Star r1           == Star r2            = r1 == r2
  Epsilon           == Epsilon            = True
  _                 == _                  = False

-- | The RegexI type represents the structure of the regular expression that
--   corresponds to a given Glushkov automaton. The children of the automaton
--   in the StarI, UnionI, and ConcatI cases represent the sub-automatons that
--   are used to evaluate different parts of the regular expression for a
--   particular input.
data RegexI = SymbolI Char
            | UnionI RegInfo RegInfo
            | ConcatI RegInfo RegInfo
            | StarI RegInfo
            | EpsilonI
      deriving (Show, Eq)

-- | The RegInfo type represents a Glushkov automaton. It stores information
--   about the state in i, and in r, it stores the structure of the underlying
--   regular expression.
data RegInfo = RegInfo { r :: RegexI, i :: Info } deriving (Show, Eq)

-- | Check if the given string is matched by the given regex.
match :: RegInfo -> String -> Bool
match reg []     = empty reg
match reg (c:cs) = final $ foldl (shift False) (shift True reg c) cs

-- | Check if the given regex accepts the empty string.
emptyR :: Regex -> Bool
emptyR  Epsilon       = True
emptyR (Union r1 r2)  = emptyR r1 || emptyR r2
emptyR (Concat r1 r2) = emptyR r1 && emptyR r2
emptyR (Star _)       = True
emptyR (Symbol _)     = False

-- | Convert a regex to a Glushkov automaton in its initial state.
regToRI :: Regex -> RegInfo
regToRI (Symbol c)          = RegInfo { r = SymbolI c, i = Info False False }
regToRI (Union r1 r2)       = RegInfo { r = UnionI (regToRI r1) (regToRI r2),
                                        i = Info False (emptyR r1 || emptyR r2) }
regToRI (Concat r1 Epsilon) = regToRI r1
regToRI (Concat r1 r2)      = RegInfo { r = ConcatI (regToRI r1) (regToRI r2),
                                        i = Info False (emptyR r1 && emptyR r2) }
regToRI (Star rg)           = RegInfo { r = StarI $ regToRI rg, 
                                        i = Info False True }
regToRI Epsilon             = RegInfo { r = EpsilonI , i = Info False True }

-- | Check if a Glushkov automaton accepts the empty string.
empty :: RegInfo -> Bool
empty = isEmpty . i

-- | Check if a Glushkov automaton is an accept state.
final :: RegInfo -> Bool
final = isFinal . i

-- | Simulate the transition function of a Glushkov automaton.
shift :: Bool -> RegInfo -> Char -> RegInfo
shift b reg c = 
  case r reg of
       EpsilonI    -> RegInfo EpsilonI $ Info False True
       SymbolI x   -> RegInfo (SymbolI x) $ Info (b && c == x) False
       UnionI x y  -> let (x', y') = (shift b x c, shift b y c) in
                        RegInfo (UnionI x' y') $ 
                          Info (final x' || final y') (empty x' || empty y')
       ConcatI x y -> 
         let (x',y') = (shift b x c, shift (b && empty x || final x) y c) in
                        RegInfo (ConcatI x' y') $ 
                          Info (final y' || (final x' && empty y')) $
                            empty x' && empty y'
       StarI x     -> let x' = shift (b || final x) x c in
                        RegInfo (StarI x') $ Info (final x') True