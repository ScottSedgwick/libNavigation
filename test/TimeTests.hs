module TimeTests where
    
import Test.HUnit
import TestUtils
import LibNav

type TestDatum = (Degrees, Int)

testData = [TestDatum]
testData = [ (dmToDeg ((-62), 51), 4)
           , (dmToDeg ((-69), 12), 5)
           , (dmToDeg (45, 36), (-3))
           , (dmToDeg (164, 54), (-11))
           ]

mkTimeTest :: TestDatum -> Test
mkTimeTest = map f testData
  where
    f (deg, h) = h ~=? zoneNumber deg