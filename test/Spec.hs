import Test.HUnit
import MoveTests
import GcTests

tests = TestList 
    [ TestLabel "Movement Tests" moveTests
    , TestLabel "Great Circle Tests" gcTests
    ]

main :: IO ()
main = do
    runTestTT tests
    return ()
