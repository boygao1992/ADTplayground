module FRP.TimeFlies.SignalVectors where

import Data.Exists (Exists, mkExists, runExists)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

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

sample :: forall a. a -> Sample a (SVProxy (Signal a))
sample = SVSample

sampleEvt :: forall a. Sample a (SVProxy (Event a))
sampleEvt = SVSampleEvent

sampleNothing :: forall a. Sample a (SVProxy Empty)
sampleNothing = SVSampleEmpty

combineSamples
  :: forall a svLeft svRight
   . Sample a (SVProxy svLeft)
  -> Sample a (SVProxy svRight)
  -> Sample a (SVProxy (Append svLeft svRight))
combineSamples l r = SVSampleBoth (mkExists l) (mkExists r)

-- splitSamples
--   :: forall a svLeft svRight
--    . Sample a (SVProxy (Append svLeft svRight))
--   -> Tuple (Sample a (SVProxy svLeft)) (Sample a (SVProxy svRight))
-- splitSamples (SVSampleBoth l r) =
--   Tuple (runExists unsafeCoerce l) (runExists unsafeCoerce r)
