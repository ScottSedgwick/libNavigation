module LibNav.GreatCircle where

-- The math and worked examples for this unit taken from:
-- http://shipofficer.com/so/wp-content/uploads/2015/02/10.-Great-Circle-Sailing.pdf
-- A copy of this PDF is in the docs folder.

import LibNav.Types

gcDistance :: Posn -> Posn -> NMiles
gcDistance posnA posnB = 60 * d
    where
        (Deg d) = gcArc posnA posnB

gcArc :: Posn -> Posn -> Degrees
gcArc posnA posnB = acos $ sin (lat posnA) * sin (lat posnB) +
                           cos (lat posnA) * cos (lat posnB) * cos (lon posnB - lon posnA)

getDir :: Degrees -> Degrees -> EW
getDir (Deg a) (Deg b) 
    | abs (b - a) > 180 = if a < b then W else E
    | a < b             = E
    | otherwise         = W

gcInitQuadrant :: Posn -> Posn -> Quadrant
gcInitQuadrant a b  
    | lat a > Deg 0 = if d == E then NE else NW
    | d == E = SE
    | otherwise = SW
    where d = getDir (lon a) (lon b)

gcFinalQuadrant :: Posn -> Posn -> Quadrant
gcFinalQuadrant a b 
    | lat a > (Deg 0) = if d == E then SE else SW
    | d == E = NE
    | otherwise = NW
    where d = getDir (lon a) (lon b)

gcQuadAdjust :: Quadrant -> Degrees -> Degrees
gcQuadAdjust q (Deg r) = Deg $
    case q of
        NE -> r
        SE -> 180 - r
        NW -> 360 - r
        SW -> 180 + r
        
gcCourse :: (Degrees -> Degrees -> Degrees -> Degrees) -> (Posn -> Posn -> Quadrant) -> Posn -> Posn -> Degrees
gcCourse f g posnA posnB = gcQuadAdjust quad rawCrs
    where
        dAB     = gcArc posnA posnB
        (Deg latAraw) = lat posnA
        (Deg latBraw) = lat posnB
        latA    = abs latAraw
        latB    = (if ((latAraw > 0) && (latBraw < 0)) || ((latAraw < 0) && (latBraw > 0)) then (-1) else 1) * abs latBraw
        rawCrs  = f (Deg latA) (Deg latB) dAB
        quad    = g posnA posnB
        
gcInitCourse :: Posn -> Posn -> Degrees
gcInitCourse = gcCourse f gcInitQuadrant
    where 
        f latA latB dAB = acos $ (sin latB - sin latA * cos dAB) / (cos latA * sin dAB)
    
gcFinalCourse :: Posn -> Posn -> Degrees
gcFinalCourse = gcCourse f gcFinalQuadrant
    where
        f latA latB dAB = acos $ (sin latA - sin latB * cos dAB) / (cos latB * sin dAB)
