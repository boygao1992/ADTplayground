module FRP.TimeFlies.SignalVectors where

import Data.Leibniz (type (~))

foreign import kind SV
foreign import data Empty :: SV
foreign import data Signal :: Type -> SV
foreign import data Event :: Type -> SV
foreign import data Append :: SV -> SV -> SV

data SVProxy (sv :: SV) = SVProxy

data SVSample a
  = SVSample a (a ~ SVProxy (Signal a))
  | SVSampleEvent (a ~ SVProxy (Event a))
  | SVSampleEmpty (a ~ SVProxy Empty)
    -- NOTE cannot be simulated sorely by Leibniz Equality
  -- | SVSampleBoth (SVSample (a ~ SVProxy svLeft)) (SVSample (a ~ SVProxy svRight)) (a ~ SVProxy (Append svLeft svRight))

