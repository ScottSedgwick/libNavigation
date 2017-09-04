module LibNav.Types where

type Degrees = Double
type Radians = Double
type Knots = Double
type Lat = Degrees
type Lon = Degrees
type LatR = Radians
type LonR = Radians
type Hours = Double
type NMiles = Double
data Quadrant = NE | SE | NW | SW deriving (Eq, Show)
data EW = E | W deriving (Eq, Show)

data Posn = Posn { lat :: Lat, lon :: Lon } deriving (Show)
instance Eq Posn where
  (==) p1 p2 = ((abs (la1 - la2)) < err) && ((abs (lo1 - lo2)) < err)
    where
      la1 = lat p1
      la2 = lat p2
      lo1 = lon p1
      lo2 = lon p2
      err = 0.000002
      
data PosnR = PosnR { latR :: LatR, lonR :: LonR } deriving (Show)
instance Eq PosnR where
    (==) p1 p2 = ((abs (la1 - la2)) < err) && ((abs (lo1 - lo2)) < err)
        where
        la1 = latR p1
        la2 = latR p2
        lo1 = lonR p1
        lo2 = lonR p2
        err = 0.00000002
      
position :: Integer -> Double -> Integer -> Double -> Posn
position latDeg latMin lonDeg lonMin = Posn latitude longtitude
    where
        latitude   = (fromIntegral latDeg :: Double) + ((latMin / 60) * if (latDeg < 0) then (-1) else 1)
        longtitude = (fromIntegral lonDeg :: Double) + ((lonMin / 60) * if (lonDeg < 0) then (-1) else 1)
