module LibNav.PlaneSailing where

import LibNav.GreatCircle
import LibNav.Types

planeQuadrant :: Posn -> Posn -> Quadrant
planeQuadrant p1 p2
  | lat p1 < lat p2 = -- N
    if lon p1 < lon p2
    then if (lon p2 - lon p1) > Deg 180 then NW else NE
    else if (lon p1 - lon p2) > Deg 180 then NE else NW
  | lon p1 < lon p2 = if (lon p2 - lon p1) > Deg 180 then SW else SE
  | (lon p1 - lon p2) > Deg 180 = SE
  | otherwise = SW

planeCourse :: Posn -> Posn -> Degrees
planeCourse p1 p2 = gcQuadAdjust (planeQuadrant p1 p2) rawCrs
    where
        dLat    = abs $ lat p1 - lat p2
        dl      = abs $ lon p1 - lon p2
        dLon    = abs $ if dl > 180 then 360 - dl else dl
        meanLat = (lat p1 + lat p2) / 2
        dep     = dLon * cos meanLat
        rawCrs  = atan $ dep / dLat


planeDistance :: Posn -> Posn -> NMiles
planeDistance p1 p2 = abs $ 60 * dLat / c
    where
        (Deg dLat) = lat p1 - lat p2
        (Deg c) = cos $ planeCourse p1 p2

planeDR :: Posn -> Degrees -> NMiles -> Posn
planeDR p c d = Posn { lat = fLat, lon = fLon }
    where
        dLat = (Deg d / 60) * cos c
        fLat = lat p + dLat
        dep  = dLat * tan c
        meanLat = lat p + (dLat / 2)
        dLon = dep / cos meanLat
        l    = lon p + dLon
        fLon = if l > 180 then l - 360 else l