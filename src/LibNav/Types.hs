module LibNav.Types where

mantissa :: Double -> Double
mantissa x = x - fromIntegral (round x :: Integer)

tau :: Double
tau = pi * 2

degToRad :: Degrees -> Radians
degToRad (Deg d) = Rad $ tau * mantissa (d / 360)

radToDeg :: Radians -> Degrees
radToDeg (Rad r) = Deg $ (r / tau) * 360

newtype Degrees = Deg Double deriving (Eq, Ord, Show)
instance Num Degrees where
    (+) (Deg a) (Deg b) = Deg (a + b)
    (-) (Deg a) (Deg b) = Deg (a - b)
    (*) (Deg a) (Deg b) = Deg (a * b)
    abs (Deg a) = Deg $ abs a
    signum (Deg a) = Deg $ signum a
    fromInteger a = Deg $ fromInteger a
instance Fractional Degrees where
    fromRational x = Deg $ fromRational x
    (/) (Deg a) (Deg b) = Deg $ a / b
instance Floating Degrees where
    pi = Deg pi
    exp (Deg x) = Deg $ exp x
    log (Deg x) = Deg $ log x
    sin = radToDeg . sin . degToRad
    cos = radToDeg . cos . degToRad
    asin = radToDeg . asin . degToRad
    acos = radToDeg . acos . degToRad
    atan = radToDeg . atan . degToRad
    sinh = radToDeg . sinh . degToRad
    cosh = radToDeg . cosh . degToRad
    asinh = radToDeg . asinh . degToRad
    acosh = radToDeg . acosh . degToRad
    atanh = radToDeg . atanh . degToRad

newtype Radians = Rad Double deriving (Eq, Ord, Show)
instance Num Radians where
    (+) (Rad a) (Rad b) = Rad (a + b)
    (-) (Rad a) (Rad b) = Rad (a - b)
    (*) (Rad a) (Rad b) = Rad (a * b)
    abs (Rad a) = Rad $ abs a
    signum (Rad a) = Rad $ signum a
    fromInteger a = Rad $ fromInteger a
instance Fractional Radians where
    fromRational x = Rad $ fromRational x
    (/) (Rad a) (Rad b) = Rad $ a / b
instance Floating Radians where
    pi = Rad pi
    exp (Rad x) = Rad $ exp x
    log (Rad x) = Rad $ log x
    sin (Rad x) = Rad $ sin x
    cos (Rad x) = Rad $ cos x
    asin (Rad x) = Rad $ asin x
    acos (Rad x) = Rad $ acos x
    atan (Rad x) = Rad $ atan x
    sinh (Rad x) = Rad $ sinh x
    cosh (Rad x) = Rad $ cosh x
    asinh (Rad x) = Rad $ asinh x
    acosh (Rad x) = Rad $ acosh x
    atanh (Rad x) = Rad $ atanh x

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
      (Deg la1) = lat p1
      (Deg la2) = lat p2
      (Deg lo1) = lon p1
      (Deg lo2) = lon p2
      err = 0.000002

data PosnR = PosnR { latR :: LatR, lonR :: LonR } deriving (Show)
instance Eq PosnR where
    (==) p1 p2 = ((abs (la1 - la2)) < err) && ((abs (lo1 - lo2)) < err)
        where
        (Rad la1) = latR p1
        (Rad la2) = latR p2
        (Rad lo1) = lonR p1
        (Rad lo2) = lonR p2
        err = 0.00000002

position :: Integer -> Double -> Integer -> Double -> Posn
position latDeg latMin lonDeg lonMin = Posn (Deg latitude) (Deg longtitude)
    where
        latitude   = (fromIntegral latDeg :: Double) + ((latMin / 60) * if (latDeg < 0) then (-1) else 1)
        longtitude = (fromIntegral lonDeg :: Double) + ((lonMin / 60) * if (lonDeg < 0) then (-1) else 1)
