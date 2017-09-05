module LibNav.Horizon where

import LibNav.Types

visibleHorizon :: Metres -> NMiles
visibleHorizon m = 2.07 * sqrt m

radarHorizon :: Metres -> NMiles
radarHorizon m = 2.21 * sqrt m