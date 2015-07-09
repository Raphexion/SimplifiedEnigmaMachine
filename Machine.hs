module Machine where

import EnigmaChars
import Wheel
import Wheels

import Control.Monad
import Control.Monad.State

-- | An enigma machine is composed of wheels and their positions
data Machine = Machine [Wheel] WheelPositions deriving (Show, Eq)

-- |

encode :: EnigmaChar -> State Machine EnigmaChar
encode c = do
  (Machine w p) <- get
  put (Machine w (incWheelPositions p))
  return $ multiWheel c w p

-- |

decode :: EnigmaChar -> State Machine EnigmaChar
decode c = do
  (Machine w p) <- get
  put (Machine w (incWheelPositions p))
  return $ multiWheelRev c w p

-- |

encodeMessage :: [EnigmaChar] -> State Machine [EnigmaChar]
encodeMessage xs = mapM encode xs

-- |

decodeMessage :: [EnigmaChar] -> State Machine [EnigmaChar]
decodeMessage xs = mapM decode xs
