module LibNav.Types where

mantissa :: Double -> Double
mantissa x = x - fromIntegral (round x :: Integer)

tau :: Double
tau = pi * 2

d2r :: Double -> Double
d2r d = tau * d / 360

r2d :: Double -> Double
r2d r = r * 360 / tau

newtype Degrees = Deg Double deriving (Ord, Show)
instance Eq Degrees where
    (==) (Deg a) (Deg b) = abs (b - a) < 0.001
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
    sin (Deg x) = Deg $ sin $ d2r x
    cos (Deg x) = Deg $ cos $ d2r x
    asin (Deg x) = Deg $ r2d $ asin x
    acos (Deg x) = Deg $ r2d $ acos x
    atan (Deg x) = Deg $ r2d $ atan x
    sinh (Deg x) = Deg $ sinh $ d2r x
    cosh (Deg x) = Deg $ cosh $ d2r x
    asinh (Deg x) = Deg $ r2d $ asinh x
    acosh (Deg x) = Deg $ r2d $ acosh x
    atanh (Deg x) = Deg $ r2d $ atanh x

type Knots = Double
type Lat = Degrees
type Lon = Degrees
type Hours = Double
type NMiles = Double
type Metres = Double
data Quadrant = NE | SE | NW | SW deriving (Eq, Show)
data EW = E | W deriving (Eq, Show)

data Posn = Posn { lat :: Lat, lon :: Lon } deriving (Show)
instance Eq Posn where
  (==) p1 p2 = (lat p1 == lat p2) && (lon p1 == lon p2)

position :: Integer -> Double -> Integer -> Double -> Posn
position latDeg latMin lonDeg lonMin = Posn (Deg latitude) (Deg longtitude)
    where
        latitude   = (fromIntegral latDeg :: Double) + ((latMin / 60) * if latDeg < 0 then (-1) else 1)
        longtitude = (fromIntegral lonDeg :: Double) + ((lonMin / 60) * if lonDeg < 0 then (-1) else 1)
