module MoveTests ( moveTests ) where

import Test.HUnit
import LibNav

-- TODO: Check the math, make sure it is right.  How?
-- TODO: Flesh this out with some more pre-calculated tests.

startPosn00 :: Posn
startPosn00 = Posn { lat = 0, lon = 0 }

startPosn45 :: Posn
startPosn45 = Posn { lat = 45, lon = 0 }

spd12 :: Knots
spd12 = 12

spd16 :: Knots
spd16 = 16.97056  -- 12 * sqrt 2

testMove :: Posn -> Degrees -> Knots -> Hours -> Posn -> Test
testMove origin bearing speed time expected = TestCase (assertEqual "" expected actual)
  where actual = deadReckon bearing speed time origin

testMoveN00 :: Test
testMoveN00 = testMove startPosn00 000 spd12 1 Posn { lat =  00.2, lon =  00.0 }

testMoveS00 :: Test
testMoveS00 = testMove startPosn00 180 spd12 1 Posn { lat = -00.2, lon =  00.0 }

testMoveE00 :: Test
testMoveE00 = testMove startPosn00 090 spd12 1 Posn { lat =  00.0, lon =  00.2 }
    
testMoveW00 :: Test
testMoveW00 = testMove startPosn00 270 spd12 1 Posn { lat =  00.0, lon = -00.2 }

testMoveNE0 :: Test
testMoveNE0 = testMove startPosn00 045 spd16 1 Posn { lat =  00.2, lon =  00.2 }

testMoveN45 :: Test
testMoveN45 = testMove startPosn45 000 spd12 1 Posn { lat =  45.2, lon =  00.0 }
    
testMoveE45 :: Test
testMoveE45 = testMove startPosn45 090 spd12 1 Posn { lat =  45.0, lon =  00.282843 }

moveTests :: Test
moveTests = TestList 
        [ TestLabel "Move E  at  0 degrees" testMoveE00
        , TestLabel "Move W  at  0 degrees" testMoveW00
        , TestLabel "Move N  at  0 degrees" testMoveN00
        , TestLabel "Move S  at  0 degrees" testMoveS00
        , TestLabel "Move NE at  0 degrees" testMoveNE0
        , TestLabel "Move N  at 45 degrees" testMoveN45
        , TestLabel "Move E  at 45 degrees" testMoveE45
        ]