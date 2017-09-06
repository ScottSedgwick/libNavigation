module LibNav
    ( --deadReckon
      departure
    , gcDistance
    , gcInitCourse
    , gcFinalCourse
    , planeCourse
    , planeDistance
    , radarHorizon
    , visibleHorizon
    , Posn (..)
    , Degrees(..)
    , Knots
    , Hours
    , NMiles

    -- The remainder of these exports are only included for unit testing.
    , getDir
    , gcInitQuadrant
    , gcFinalQuadrant
    , position
    , EW (..)
    , Quadrant (..)
    )
where

-- This site is an excellent resource for all things relating to seamanship:
-- http://shipofficer.com/so/

import LibNav.Types
import LibNav.GreatCircle
import LibNav.Horizon
import LibNav.MathUtils
import LibNav.ParallelSailing
import LibNav.PlaneSailing

-- deadReckon :: Degrees -> Knots -> Hours -> Posn -> Posn
-- deadReckon deg spd time posn = Posn latitude' longtitude'
--   where
--     latitude    = lat posn
--     longtitude  = lon posn
--     distance    = spd * time
--     departure   = (distance / 60) * sine deg
--     distLat     = (distance / 60) * cosine deg
--     latitude'   = latitude + distLat
--     meanLatCorr = abs (cosine ((latitude + latitude') / 2))
--     longtitude' = longtitude + (departure / meanLatCorr)
