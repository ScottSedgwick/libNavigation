module LibNav.ParallelSailing where

import LibNav.Types

departure :: Degrees -> Degrees -> Degrees -> NMiles
departure latitude lon1 lon2 = 60 * d 
    where 
        (Deg d) = abs $ dlon * cos latitude
        dlon'   = abs $ lon2 - lon1
        dlon    = if dlon' > (Deg 180) then (Deg 360) - dlon' else dlon'