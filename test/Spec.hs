import Test.HUnit
import MoveTests
import GcTests
import HorizonTests
import ParallelSailingTests
import LibNav
import TestUtils

tests = TestList 
    [ TestLabel "Movement Tests" moveTests
    , TestLabel "Great Circle Tests" gcTests
    , TestLabel "Horizon Tests" horizonTests
    , TestLabel "Parallel Sailing Tests" parallelSailingTests
    , TestCase $ assertEquals "" 0.01 0.8158 (cos (Deg 35.33333333))
    ]

main :: IO ()
main = do
    runTestTT tests
    return ()
