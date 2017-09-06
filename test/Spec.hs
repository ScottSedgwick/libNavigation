import Test.HUnit
import MoveTests
import GcTests
import HorizonTests
import ParallelSailingTests
import PlaneSailingTests
import LibNav
import TestUtils

tests = TestList 
    [ TestLabel "Movement Tests" moveTests
    , TestLabel "Great Circle Tests" gcTests
    , TestLabel "Horizon Tests" horizonTests
    , TestLabel "Parallel Sailing Tests" parallelSailingTests
    , TestLabel "Plane Sailing Tests" planeSailingTests
    ]

main :: IO ()
main = do
    runTestTT tests
    return ()
