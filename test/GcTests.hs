module GcTests ( gcTests ) where
    
import Control.Monad (unless)
import Test.HUnit
import TestUtils
import LibNav

-- The math and worked examples for these tests taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/10.-Great-Circle-Sailing.pdf
-- A copy of this PDF is in the docs folder.

errDist :: Double
errDist = 0.1

errHead :: Degrees
errHead = Deg 0.1

class MyAssertable a where
    assertEq :: String -> a -> a -> a -> Assertion

instance MyAssertable Degrees where
    assertEq s (Deg a) (Deg b) (Deg c) = assertEquals s a b c

type TestData = (String, Posn, Posn, NMiles, Degrees, Quadrant, Degrees, Quadrant, EW)

testData :: [TestData]
testData = 
    [ ("Test Case 1", position 56 20 (-8) 12,   position 52 12 (-57) 10,     1696.5, (Deg 282.6), NW, (Deg 242.0), SW, W)
    , ("Test Case 2", position (-33) 22 113 8,  position (-10) 51 49 16,     3738.1, (Deg 275.2), SW, (Deg 302.1), NW, W)
    , ("Test Case 3", position 49 12 (-122) 50, position 13 30 145 15,       4863.4, (Deg 280.3), NW, (Deg 000.0), NE, W)    -- Crosses meridian
    , ("Test Case 4", position (-46) 20 169 10, position (-26) 25 (-105) 15, 4099.1, (Deg 106.1), SE, (Deg 000.0), NE, E)    -- Crosses meridian
    , ("Test Case 5", position (-17) 0 170 0,   position 22 0 (-110) 0,      5247.2, (Deg  66.1), SE, (Deg 070.5), NE, E)    -- Crosses meridian and equator
    ]
    
mkDistTest :: TestData -> Test
mkDistTest (s, pA, pB, d, ih, iq, fh, fq, idir) = TestCase (assertEquals ("Distance Incorrect: " ++ s) errDist d (gcDistance pA pB))

mkInitCourseTest :: TestData -> Test
mkInitCourseTest (s, pA, pB, d, ih, iq, fh, fq, idir) = TestCase (assertEq ("Initial Course Incorrect: " ++ s) errHead ih (gcInitCourse pA pB))

mkInitQuadTest :: TestData -> Test
mkInitQuadTest (s, pA, pB, d, ih, iq, fh, fq, idir) = TestCase (assertEqual ("Initial Quadrant Incorrect: " ++ s) iq (gcInitQuadrant pA pB))

mkEWTest :: TestData -> Test
mkEWTest (s, pA, pB, d, ih, iq, fh, fq, idir) = TestCase (assertEqual ("Initial Direction Incorrect: " ++ s) idir (getDir (lon pA) (lon pB)))

mkFinalQuadTest :: TestData -> Test
mkFinalQuadTest (s, pA, pB, d, ih, iq, fh, fq, idir) = TestCase (assertEqual ("Final Quadrant Incorrect: " ++ s) fq (gcFinalQuadrant pA pB))

mkFinalQuadTests :: [TestData] -> [Test]
mkFinalQuadTests [] = []
mkFinalQuadTests ((s, pA, pB, d, ih, iq, fh, fq, idir) : tds) = if fh == (Deg 0.0) then ts else (t : ts)
    where
        t  = mkFinalQuadTest (s, pA, pB, d, ih, iq, fh, fq, idir)
        ts = mkFinalQuadTests tds

mkFinalCourseTest :: TestData -> Test
mkFinalCourseTest (s, pA, pB, d, ih, iq, fh, fq, idir) = TestCase (assertEq ("Final Course Incorrect: " ++ s) errHead fh (gcFinalCourse pA pB))

mkFinalCourseTests :: [TestData] -> [Test]
mkFinalCourseTests [] = []
mkFinalCourseTests ((s, pA, pB, d, ih, iq, fh, fq, idir) : tds) = if fh == (Deg 0.0) then ts else (t : ts)
    where
        t  = mkFinalCourseTest (s, pA, pB, d, ih, iq, fh, fq, idir)
        ts = mkFinalCourseTests tds

gcTests :: Test
gcTests = TestList 
    [ TestLabel "Distance Tests" $ TestList $ map mkDistTest testData 
    , TestLabel "Initial Course Tests" $ TestList $ map mkInitCourseTest testData
    , TestLabel "Initial Quadrant Tests" $ TestList $ map mkInitQuadTest testData
    , TestLabel "Initial Direction Tests" $ TestList $ map mkEWTest testData
    , TestLabel "Final Quadrant Tests" $ TestList $ mkFinalQuadTests testData
    , TestLabel "Final Course Tests" $ TestList $ mkFinalCourseTests testData
    ]