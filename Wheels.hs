-- | Module to handle multiple layers of wheels and positions

module Wheels where

import EnigmaChars
import Wheel

type WheelPositions = Int

-- |

incWheelPositions :: WheelPositions -> WheelPositions
incWheelPositions = (+1)

-- |

multiWheel' :: EnigmaChar -> [Wheel] -> [Int] -> EnigmaChar
multiWheel' c [] _          = c
multiWheel' c _  []         = c
multiWheel' c (w:ws) (p:ps) = multiWheel' c'' ws ps
  where c'  = safeRotate p c
        c'' = wheelThrough w c'

multiWheel :: EnigmaChar -> [Wheel] -> Int -> EnigmaChar
multiWheel ec ws p = multiWheel' ec ws (valToEnigmaBase (length ws) p)

-- |

multiWheelRev' :: EnigmaChar -> [Wheel] -> [Int] -> EnigmaChar
multiWheelRev' c [] _          = c
multiWheelRev' c _  []         = c
multiWheelRev' c (w:ws) (p:ps) = multiWheelRev' c'' ws ps
  where c'  = wheelThrough w c
        c'' = safeRotate (negate p) c'

multiWheelRev :: EnigmaChar -> [Wheel] -> Int -> EnigmaChar
multiWheelRev ec ws p = multiWheelRev' ec ws_rev ps_rev
  where ws_rev = reverse ws
        ps_rev = reverse (valToEnigmaBase (length ws) p)
