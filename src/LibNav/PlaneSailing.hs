module LibNav.PlaneSailing where

import LibNav.GreatCircle
import LibNav.ParallelSailing
import LibNav.Types

planeQuadrant :: Posn -> Posn -> Quadrant
planeQuadrant p1 p2 = 
    if lat p1 < lat p2
    then -- N
        if lon p1 < lon p2 then NE else NW
    else
        if lon p1 < lon p2 then SE else SW

planeCourse :: Posn -> Posn -> Degrees
planeCourse p1 p2 = gcQuadAdjust (planeQuadrant p1 p2) rawCrs
    where
        dLat    = abs $ lat p1 - lat p2
        dLon    = abs $ lon p1 - lon p2
        meanLat = (lat p1 + lat p2) / 2
        dep     = dLon * cos meanLat
        rawCrs  = atan $ dep / dLat
            

planeDistance :: Posn -> Posn -> NMiles
planeDistance p1 p2 = abs $ 60 * dLat / c
    where
        (Deg dLat) = lat p1 - lat p2
        (Deg c) = cos $ planeCourse p1 p2