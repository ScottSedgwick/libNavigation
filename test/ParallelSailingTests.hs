module ParallelSailingTests ( parallelSailingTests ) where
    
import Control.Monad (unless)
import Test.HUnit
import TestUtils
import LibNav

-- Unit tests from http://shipofficer.com/so/wp-content/uploads/2015/02/5.-Parallel-Sailing.pdf

type TestData = (String, Posn, Posn, NMiles)

errDist :: NMiles
errDist = 0.03

testData :: [TestData]
testData = [ ("Test Case 1", position 35 20 (-15) 31,   position 35 20 (-25) 50, 505)
           , ("Test Case 2", position (-30) 0 (-171) 0, position (-30) 0 178 0,  571.6)
           ]

mkTest :: TestData -> Test
mkTest (s, p1, p2, d) = TestCase (assertEquals ("Departure Incorrect: " ++ s) errDist d (departure (lat p1) (lon p1) (lon p2)))

parallelSailingTests :: Test
parallelSailingTests = TestList $ map mkTest testData