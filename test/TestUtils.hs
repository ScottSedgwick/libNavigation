module TestUtils where

import Control.Monad (unless)
import Data.CallStack
import Test.HUnit

assertEquals :: (HasCallStack, Ord a, Num a, Show a) => String -> a -> a -> a -> Assertion
assertEquals preface delta expected actual = unless (abs (expected - actual) < delta) (assertFailure msg)
    where msg = (if null preface then "" else preface ++ "\n") ++ "expected: " ++ show expected ++ "\n but got: " ++ show actual