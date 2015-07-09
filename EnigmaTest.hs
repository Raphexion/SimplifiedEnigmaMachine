import EnigmaChars
import Wheel
import Wheels
import Machine

import Control.Monad.State

wheel__I = wheelFromList [ (A,B), (B,A), (C,D), (D,C) ]
wheel_II = wheelFromList [ (A,C), (C,A), (B,D), (D,B) ]

testMachine = Machine [wheel__I, wheel_II] 0

plain_org = [A, A, A, A, B, B, B, B, C, C, C, C, D, D, D, D]
cypher    = fst $ runState (encodeMessage plain_org) testMachine
plain_rev = fst $ runState (decodeMessage cypher   ) testMachine
