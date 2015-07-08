-- | An Enigma wheel maps a enigma character to another
module Wheel where

import EnigmaChars
import qualified Data.Map as M

-- | The main purpose of a wheel is to map a character to another,
type Wheel         = M.Map EnigmaChar EnigmaChar
