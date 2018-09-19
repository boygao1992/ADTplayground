module Slot where

import Prelude
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import OrdBox (OrdBox, mkOrdBox)

{- | SProxy: an isomorphism between a value (Unit / SProxy) and a type-level symbol -}

-- | Symbol kind is defined in the Prim, whose value-level representation is a String
-- data SProxy (sym :: Symbol) = SProxy

-- class IsSymbol (sym :: Symbol) where
--   reflectSymbol :: SProxy sym -> String

foreign import data Any :: Type -- literally, represents any Type which means the type information upon construction was thrown away

-- | f = Algebra Functor of the child Component
-- | o = Output of the child Component
-- | p = a unique key to identify the type of child Component (!! not instances of the type !!) in the SlotStorage
-- | no data constructor thus no data/value-level representation
-- | solely for type-level coordination
data Slot (f :: Type -> Type) o p

-- | slot = type-level function/constructor that carries two Types (one of Arrow kind, one of Type kind)
newtype SlotStorage (ps :: # Type) (slot :: (Type -> Type) -> Type -> Type) =
  SlotStorage (Map (Tuple String (OrdBox Any)) Any)

lookup
  :: forall sym px ps slot f o p
  -- Row.Cons =
  --   class Cons (sym :: Symbol) (new :: Type) (px :: # Type) (ps :: # Type)
  --     | sym new px -> ps
  --     , sym ps -> new px
   . Row.Cons sym (Slot f o p) px ps -- append Type `Slot f o p` to a Row `px` which is stored in a type-level Map (key-value pair: Symbol -> # Type).  (A type class dictionary exists before and out of the program but can be accessed and manipulated by the program at compile time.)
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> SlotStorage ps slot
  -> Maybe (slot f o)
lookup sym key (SlotStorage smap) =
  smap
      # Map.lookup (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key)))
    >>> coerceSlot
  where
    coerceSlot :: Maybe Any -> Maybe (slot f o)
    coerceSlot = unsafeCoerce -- get Type f and o out of Row dictionary and replace Any

    coerceBox :: OrdBox p -> OrdBox Any
    coerceBox = unsafeCoerce -- throw away Type p because it's not useful afterward. We only need its ability to compare with other p (through `eq` and `compare`) which is encapsulated in OrdBox data-type

