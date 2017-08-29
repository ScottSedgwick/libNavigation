import Test.HUnit
import MoveTests

tests = TestList 
    [ TestLabel "Movement Tests" moveTests
    ]

main :: IO ()
main = do
    runTestTT tests
    return ()
