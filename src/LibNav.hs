module LibNav
    ( --deadReckon
      departure
    , gcDistance
    , gcInitCourse
    , gcFinalCourse
    , planeCourse
    , planeDistance
    , planeDR
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
    , gcQuadAdjust
    , planeQuadrant
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
import LibNav.ParallelSailing
import LibNav.PlaneSailing

