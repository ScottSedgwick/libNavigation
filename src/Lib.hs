module Lib
    ( move
    , Posn(..)
    , Degrees
    , Knots
    , Hours
    ) 
where

type Degrees = Double
type Radians = Double
type Knots = Double
type Lat = Double
type Lon = Double
type Hours = Double

data Posn = Posn { lat :: Lat, lon :: Lon } deriving (Show)
instance Eq Posn where
  (==) p1 p2 = ((abs (la1 - la2)) < err) && ((abs (lo1 - lo2)) < err)
    where
      la1 = lat p1
      la2 = lat p2
      lo1 = lon p1
      lo2 = lon p2
      err = 0.000002

degToRad :: Degrees -> Radians
degToRad x = x * pi / 180

move :: Degrees -> Knots -> Hours -> Posn -> Posn
move deg spd time posn = Posn lat' lon'
  where
    latt = lat posn
    lonn = lon posn
    dist = spd * time
    dstx = (dist / 60) * sin (degToRad deg)
    dsty = (dist / 60) * cos (degToRad deg)
    lat' = latt + dsty
    latc = abs (cos (degToRad ((latt + lat') / 2)))
    lon' = lonn + (dstx / latc)

