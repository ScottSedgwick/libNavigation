module PlaneSailingTests where

import Test.HUnit
import TestUtils
import LibNav


type TestData = (Posn, Posn, Degrees, NMiles)

errDist :: NMiles
errDist = 0.05

testData :: [TestData]
testData = [ (position 27 15 (-71) 23, position 28 11 (-68) 18, Deg 071.122537, 173.1)
           , (position 35 15 (-62) 23, position 30 25 (-70) 18, Deg 233.997857, 493.4)
           ]

mkCourseTest :: TestData -> Test
mkCourseTest (p1, p2, c, d) = c ~=? planeCourse p1 p2

mkDistanceTest :: TestData -> Test
mkDistanceTest (p1, p2, c, d) = TestCase $ assertEquals "Distance Incorrect" errDist d (planeDistance p1 p2)

planeSailingTests :: Test
planeSailingTests = TestList $ courseTests ++ distanceTests
    where
        courseTests = map mkCourseTest testData
        distanceTests = map mkDistanceTest testData