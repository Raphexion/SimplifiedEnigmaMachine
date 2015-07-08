-- | An Enigma wheel maps a enigma character to another

module Wheel where

import EnigmaChars
import qualified Data.Map as M

-- | The main purpose of a wheel is to map a enigma character to another,
-- | this is how we can obscure text

type Wheel         = M.Map EnigmaChar EnigmaChar

-- | Allow rotate of enigma characters

safeRotate :: Int -> EnigmaChar -> EnigmaChar
safeRotate ticks = modEnigmaChar . (+ticks) . fromEnum
