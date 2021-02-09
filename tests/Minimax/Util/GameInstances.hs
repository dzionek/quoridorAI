{-
    Module: GameInstances.

    Needed to check equality.
-}
module Util.GameInstances where 

import Types
import Print 

instance Eq Player where 
    p1 == p2 = 
        (name p1) == (name p2) &&
        (currentCell p1) == (currentCell p2) &&
        (remainingWalls p1) == (remainingWalls p2) &&
        (winningPositions p1) == (winningPositions p2) &&
        (isHuman p1) == (isHuman p2)

instance Eq Game where 
    (Game b1 ps1) == (Game b2 ps2) = (b1 == b2) && (ps1 == ps2)

instance Show Game where 
    show (Game b ps) = printBoard' b ps
