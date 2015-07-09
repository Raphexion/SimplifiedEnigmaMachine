-- | An Enigma wheel maps a enigma character to another

module Wheel (Wheel, wheelFromList, safeRotate, wheelThrough) where

import EnigmaChars
import qualified Data.Map as M

-- | The main purpose of a wheel is to map a enigma character to another,
-- | this is how we can obscure text

type Wheel         = M.Map EnigmaChar EnigmaChar

-- | Create a wheel
wheelFromList :: [ (EnigmaChar, EnigmaChar) ] -> M.Map EnigmaChar EnigmaChar
wheelFromList = M.fromList

-- | Allow rotate of enigma characters

safeRotate :: Int -> EnigmaChar -> EnigmaChar
safeRotate ticks = modEnigmaChar . (+ticks) . fromEnum

wheelThrough :: Wheel -> EnigmaChar -> EnigmaChar
wheelThrough w c = case (M.lookup c w) of
  Just c' -> c'
  Nothing -> c
