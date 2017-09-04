module LibNav.GreatCircle where

-- The math and worked examples for this unit taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/10.-Great-Circle-Sailing.pdf
-- A copy of this PDF is in the docs folder.

import LibNav.MathUtils
import LibNav.Types

gcDistance :: Posn -> Posn -> NMiles
gcDistance posnA posnB = 60 * d
    where
        (Deg d) = gcArc posnA posnB

gcArc :: Posn -> Posn -> Degrees
gcArc posnA posnB = radToDeg $ gcArcR (posToPosR posnA) (posToPosR posnB)

gcArcR :: PosnR -> PosnR -> Radians
gcArcR posnA posnB = acos $ sin (latR posnA) * sin (latR posnB) +
                               cos (latR posnA) * cos (latR posnB) * cos (lonR posnB - lonR posnA)

getDir :: Degrees -> Degrees -> EW
getDir a b = getDirR (degToRad a) (degToRad b)

getDirR :: Radians -> Radians -> EW
getDirR (Rad a) (Rad b)
    | abs (b - a) > pi = if a < b then W else E
    | a < b            = E
    | otherwise        = W

gcInitQuadrant :: Posn -> Posn -> Quadrant
gcInitQuadrant posnA posnB = gcInitQuadrantR (posToPosR posnA) (posToPosR posnB)

gcFinalQuadrant :: Posn -> Posn -> Quadrant
gcFinalQuadrant a b = gcFinalQuadrantR (posToPosR a) (posToPosR b)

gcQuadAdjustR :: Quadrant -> Radians -> Radians
gcQuadAdjustR q (Rad r) = Rad $
    case q of
        NE -> r
        SE -> pi  - r
        NW -> tau - r
        SW -> pi  + r
        
gcCourseR :: (Radians -> Radians -> Radians -> Radians) -> (PosnR -> PosnR -> Quadrant) -> PosnR -> PosnR -> Radians
gcCourseR f g posnA posnB = gcQuadAdjustR quad rawCrs
    where
        dAB     = gcArcR posnA posnB
        (Rad latAraw) = latR posnA
        (Rad latBraw) = latR posnB
        latA    = abs latAraw
        latB    = (if ((latAraw > 0) && (latBraw < 0)) || ((latAraw < 0) && (latBraw > 0)) then (-1) else 1) * abs latBraw
        rawCrs  = f (Rad latA) (Rad latB) dAB
        quad    = g posnA posnB
        
gcInitCourse :: Posn -> Posn -> Degrees
gcInitCourse posnA posnB = radToDeg $ gcInitCourseR (posToPosR posnA) (posToPosR posnB)

gcInitCourseR :: PosnR -> PosnR -> Radians
gcInitCourseR = gcCourseR f gcInitQuadrantR
    where 
        f latA latB dAB = acos $ (sin latB - sin latA * cos dAB) / (cos latA * sin dAB)

gcInitQuadrantR :: PosnR -> PosnR -> Quadrant
gcInitQuadrantR posnA posnB
    | latR posnA > Rad 0 = if d == E then NE else NW
    | d == E = SE
    | otherwise = SW
    where d = getDirR (lonR posnA) (lonR posnB)
    
gcFinalCourse :: Posn -> Posn -> Degrees
gcFinalCourse posnA posnB = radToDeg $ gcFinalCourseR (posToPosR posnA) (posToPosR posnB)

gcFinalCourseR :: PosnR -> PosnR -> Radians
gcFinalCourseR = gcCourseR f gcFinalQuadrantR
    where
        f latA latB dAB = acos $ (sin latA - sin latB * cos dAB) / (cos latB * sin dAB)

gcFinalQuadrantR :: PosnR -> PosnR -> Quadrant
gcFinalQuadrantR posnA posnB
    | latR posnA > (Rad 0) = if d == E then SE else SW
    | d == E = NE
    | otherwise = NW
    where d = getDirR (lonR posnA) (lonR posnB)