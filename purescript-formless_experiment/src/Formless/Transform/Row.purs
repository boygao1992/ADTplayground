module Formless.Transform.Row where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal.Transform (class Row1Cons, FromScratch, fromScratch)
import Formless.Types.Form (FormProxy, InputField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Row (RLProxy(..), RProxy(..))


----------
-- Construction from rows

-- | A function to transform a row of labels into a InputFields. This allows you
-- | to go directly from a custom form newtype to an inputs record without having to
-- | fill in any values. Requires that all members have an instance of the
-- | `Initial` type class (all monoidal values do by default, along with some
-- | other primitives).
mkInputFields
  :: ∀ xs form inputs
  . RL.RowToList inputs xs
  => Newtype (form Record InputField) (Record inputs)
  => MakeInputFieldsFromRow xs inputs inputs
  => FormProxy form
  -> form Record InputField
mkInputFields _ = wrap $ fromScratch builder
  where
    builder = mkInputFieldsFromRowBuilder
      (RLProxy :: RLProxy xs)
      (RProxy :: RProxy inputs)

-- | The class that provides the Builder implementation to efficiently
-- | transform a row into a proper InputFields by wrapping it in newtypes and
-- | supplying initial values
class MakeInputFieldsFromRow (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  mkInputFieldsFromRowBuilder :: RLProxy xs -> RProxy row -> FromScratch to

instance mkInputFieldsFromRowNil :: MakeInputFieldsFromRow RL.Nil row () where
  mkInputFieldsFromRowBuilder _ _ = identity

instance mkInputFieldsFromRowCons
  :: ( IsSymbol name
     , Initial i
     , Row.Cons name (InputField e i o) trash row
     , MakeInputFieldsFromRow tail row from
     , Row1Cons name (InputField e i o) from to
     )
  => MakeInputFieldsFromRow (RL.Cons name (InputField e i o) tail) row to where
  mkInputFieldsFromRowBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = InputField initial
      rest = mkInputFieldsFromRowBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

-- | A type to collect constraints necessary to apply to prove that a record of
-- | SProxies is compatible with your form type.
type SProxies form =
    ∀ xs row inputs
    . RL.RowToList inputs xs
   => Newtype (form Record InputField) (Record inputs)
   => MakeSProxies xs row
   => Record row

-- | A helper function to produce a record of SProxies given a form spec, to save
-- | you the boilerplate of writing them all out.
mkSProxies
  :: ∀ form xs inputs row
   . RL.RowToList inputs xs
  => Newtype (form Record InputField) (Record inputs)
  => MakeSProxies xs row
  => FormProxy form
  -> Record row
mkSProxies _ = fromScratch builder
  where
    builder = makeSProxiesBuilder (RLProxy :: RLProxy xs)

-- | The class used to build up a new record of symbol proxies from an
-- | input row list.
class MakeSProxies (xs :: RL.RowList) (to :: # Type) | xs -> to where
  makeSProxiesBuilder :: RLProxy xs -> FromScratch to

instance makeSProxiesNil :: MakeSProxies RL.Nil () where
  makeSProxiesBuilder _ = identity

instance makeSProxiesCons
  :: ( IsSymbol name
     , Row1Cons name (SProxy name) from to
     , MakeSProxies tail from
     )
  => MakeSProxies (RL.Cons name x tail) to where
  makeSProxiesBuilder _ = first <<< rest
    where
      rest = makeSProxiesBuilder (RLProxy :: RLProxy tail)
      first = Builder.insert (SProxy :: SProxy name) (SProxy :: SProxy name)

-- | DEPRECATED MakeDebouncerFieldsFromRow

-- class MakeDebouncerFieldsFromRow form m where
--   mkDebouncerFields :: FormDebouncerFields form m

-- instance makeDebouncerFieldsFromRow
--   :: ( Newtype (FormDebouncerFields form m) (Record debouncers)
--     , RL.RowToList debouncers xs
--     , MakeDebouncerFieldsFromRowImpl xs debouncers
--     )
--   => MakeDebouncerFieldsFromRow form m
--   where
--     mkDebouncerFields =
--       wrap $ fromScratch (makeDebouncerFieldsFromRowImpl (RLProxy :: RLProxy xs))

-- class MakeDebouncerFieldsFromRowImpl (xs :: RL.RowList) (to :: # Type) | xs -> to where
--   makeDebouncerFieldsFromRowImpl :: RLProxy xs -> FromScratch to

-- instance makeDebouncerFieldsFromRowNil
--   :: MakeDebouncerFieldsFromRowImpl RL.Nil ()
--   where
--     makeDebouncerFieldsFromRowImpl _ = identity

-- instance makeDebouncerFieldsFromRowCons
--   :: ( MakeDebouncerFieldsFromRowImpl restRl restTo
--     , IsSymbol label
--     , Row1Cons label (DebouncerField m e i o) restTo to
--     )
--   => MakeDebouncerFieldsFromRowImpl (RL.Cons label (DebouncerField m e i o) restRl) to
--   where
--     makeDebouncerFieldsFromRowImpl _ =
--       Builder.insert
--         (SProxy :: SProxy label)
--         (DebouncerField { debouncer : Nothing, canceller : Nothing })
--       <<< makeDebouncerFieldsFromRowImpl (RLProxy :: RLProxy restRl)

