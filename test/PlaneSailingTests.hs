module PlaneSailingTests where

import Test.HUnit
import TestUtils
import LibNav


type TestData = (Posn, Posn, Degrees, NMiles, Quadrant)

errDist :: NMiles
errDist = 0.05

testData :: [TestData]
testData = [ (position 27 15 (-71) 23, position 28 11 (-68) 18, Deg 071.122537, 173.1, NE)
           , (position 35 15 (-62) 23, position 30 25 (-70) 18, Deg 233.997857, 493.4, SW)
           , (position (-40) 25 175 50, position (-35) 3.61 (-176) 5.66, Deg 050.0003565, 500.0, NE)
           ]

mkQuadTest :: TestData -> Test
mkQuadTest (p1, p2, c, d, q) = q ~=? planeQuadrant p1 p2

mkCourseTest :: TestData -> Test
mkCourseTest (p1, p2, c, d, q) = c ~=? planeCourse p1 p2

mkDistanceTest :: TestData -> Test
mkDistanceTest (p1, p2, c, d, q) = TestCase $ assertEquals "Distance Incorrect" errDist d (planeDistance p1 p2)

mkDRTest :: TestData -> Test
mkDRTest (p1, p2, c, d, q) = TestCase $ assertEqual "DR Incorrect" p2 (planeDR p1 c d)

planeSailingTests :: Test
planeSailingTests = TestList $ courseTests ++ distanceTests ++ quadrantTests ++ drTests
    where
        courseTests = map mkCourseTest testData
        distanceTests = map mkDistanceTest testData
        quadrantTests = map mkQuadTest testData
        drTests = map mkDRTest testData