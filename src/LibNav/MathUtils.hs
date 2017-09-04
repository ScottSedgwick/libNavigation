module LibNav.MathUtils where

import LibNav.Types

mantissa :: Double -> Double
mantissa x = x - fromIntegral (round x :: Integer)

tau :: Double
tau = pi * 2

degToRad :: Degrees -> Radians
degToRad x = tau * mantissa (x / 360)

radToDeg :: Radians -> Degrees
radToDeg x = (x / tau) * 360

posToPosR :: Posn -> PosnR
posToPosR p = PosnR { latR = degToRad $ lat p, lonR = degToRad $ lon p }

posRToPos :: PosnR -> Posn
posRToPos p = Posn { lat = radToDeg $ latR p, lon = radToDeg $ lonR p }