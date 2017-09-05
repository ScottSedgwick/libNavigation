module HorizonTests ( horizonTests ) where

import Control.Monad (unless)
import Test.HUnit
import TestUtils
import LibNav

-- The math and worked examples for these tests taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/3.-Distance-of-Horizon.pdf
-- A copy of this PDF is in the docs folder.

horizonTests :: Test
horizonTests = TestList 
    [ TestCase $ assertEquals "Visible Horizon Incorrect" 0.01 8.02 (visibleHorizon 15)
    , TestCase $ assertEquals "Radar Horizon Incorrect" 0.01 10.6 (radarHorizon 23)
    ]