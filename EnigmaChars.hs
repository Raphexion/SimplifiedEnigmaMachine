-- | The simplified enigma machine only handles a subset of all chars

module EnigmaChars (EnigmaChar, modEnigmaChar, charToEnigmaChar, enigmaCharToChar) where

import qualified Data.Map as Map (Map, lookup, fromList)
import Data.Char (isLower)
import Data.Tuple (swap)

-- | The internal characters

data EnigmaChar = A | B | C | D | E |
                  F | G | H | I | J |
                  K | L | M | N | O |
                  P | Q | R | S | T |
                  U | V | W | X | Y |
                  Z |
                  BLANK | DOT | COMMA | QUESTIONMARK |
                  ONE | TWO | THREE | FOUR | FIVE |
                  SIX | SEVEN | EIGHT | NINE | ZERO
                deriving (Show, Eq, Enum, Ord, Bounded)

-- | It is very useful to be able to translate a number
-- | into a enigma character. Moreover, it is very helpful
-- | to have a function that does this with "modulues"

nbOfEnigmaChars :: Int
nbOfEnigmaChars = (+1) $ fromEnum (maxBound :: EnigmaChar)

modEnigma :: Int -> Int
modEnigma = (flip mod) nbOfEnigmaChars

modEnigmaChar :: Int -> EnigmaChar
modEnigmaChar = toEnum . modEnigma


-- | Even if we could autogenerate a translation list,
-- | we write it up explicitly for readability

translation = [ ('a', A), ('A', A)
              , ('b', B), ('B', B)
              , ('c', C), ('C', C)
              , ('d', D), ('D', D)
              , ('e', E), ('E', E)

              , ('f', F), ('F', F)
              , ('g', G), ('G', G)
              , ('h', H), ('H', H)
              , ('i', I), ('I', I)
              , ('j', J), ('J', J)

              , ('k', K), ('K', K)
              , ('l', L), ('L', L)
              , ('m', M), ('M', M)
              , ('n', N), ('N', N)
              , ('o', O), ('O', O)

              , ('p', P), ('P', P)
              , ('q', Q), ('Q', Q)
              , ('r', R), ('R', R)
              , ('s', S), ('S', S)
              , ('t', T), ('T', T)

              , ('u', U), ('U', U)
              , ('v', V), ('V', V)
              , ('w', W), ('W', W)
              , ('x', X), ('X', X)
              , ('y', Y), ('Y', Y)

              , ('z', Z), ('Z', Z)

              , (' ', BLANK)
              , ('.', DOT)
              , (',', COMMA)
              , ('?', QUESTIONMARK)

              , ('0', ZERO), ('1', ONE), ('2', TWO), ('3', THREE), ('4', FOUR)
              , ('5', FIVE), ('6', SIX), ('7', SEVEN), ('8', EIGHT), ('9', NINE)
              ]

-- | Hashmaps thats maps to/from char and enigma chars

mapCharToEnigmaChar :: Map.Map Char EnigmaChar
mapCharToEnigmaChar = Map.fromList translation

mapEnigmaCharToChar :: Map.Map EnigmaChar Char
mapEnigmaCharToChar = Map.fromList $ map swap $ filter (not . isLower . fst) translation

-- | Functions to convert characters to/from char and enigma chars
-- | Please note that this is not isomorphic. We will never convert back
-- | to lower letters. E.g,
-- |   Char 'a' -> Enigma Char A -> Char 'A'
-- | whereas
-- |   Char 'A' -> Enigma Char A -> Char 'A'

charToEnigmaChar :: Char -> Maybe EnigmaChar
charToEnigmaChar c = Map.lookup c mapCharToEnigmaChar

enigmaCharToChar :: EnigmaChar -> Maybe Char
enigmaCharToChar ec = Map.lookup ec mapEnigmaCharToChar
