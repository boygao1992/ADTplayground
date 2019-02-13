module FRP.TimeFlies.SignalVectors where

import Data.Exists (Exists, mkExists)

foreign import kind SV
foreign import data Empty :: SV
foreign import data Signal :: Type -> SV
foreign import data Event :: Type -> SV
foreign import data Append :: SV -> SV -> SV

data SVProxy (sv :: SV) = SVProxy

-- data SVSample a
--   = SVSample a (a ~ SVProxy (Signal a))
--   | SVSampleEvent (a ~ SVProxy (Event a))
--   | SVSampleEmpty (a ~ SVProxy Empty)
    -- NOTE cannot be simulated sorely by Leibniz Equality
  -- | SVSampleBoth (SVSample (a ~ SVProxy svLeft)) (SVSample (a ~ SVProxy svRight)) (a ~ SVProxy (Append svLeft svRight))

data Sample a svproxy
  = SVSample a
  | SVSampleEvent
  | SVSampleEmpty
  | SVSampleBoth (Exists (Sample a)) (Exists (Sample a))

svSample :: forall a. a -> Sample a (SVProxy (Signal a))
svSample = SVSample

svSampleEvent :: forall a. Sample a (SVProxy (Event a))
svSampleEvent = SVSampleEvent

svSampleEmpty :: forall a. Sample a (SVProxy Empty)
svSampleEmpty = SVSampleEmpty

svSampleBoth
  :: forall a svLeft svRight
   . Sample a (SVProxy svLeft)
  -> Sample a (SVProxy svRight)
  -> Sample a (SVProxy (Append svLeft svRight))
svSampleBoth l r = SVSampleBoth (mkExists l) (mkExists r)
