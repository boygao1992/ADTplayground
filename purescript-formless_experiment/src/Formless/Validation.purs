module Formless.Validation where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap, wrap)
import Formless.Types.Form (DebouncerField, FormField, InputField, InputFunction, OutputField, U)
import Heterogeneous.Mapping (class MapRecordWithIndex, class Mapping, ConstMapping, hmap)
import Prim.RowList (class RowToList)
import Data.Variant (Variant)

-----------
-- Synonyms

type FormInputField form = form Variant InputField
type FormInputFields form = form Record InputField

type FormOutputFields form = form Record OutputField

type FormValidationAction form = form Variant U

type FormInputFunction form = form Variant InputFunction
type FormInputFunctions form = form Record InputFunction

type FormFields form = form Record FormField

type FormValidators form m = form Record (Validation form m)

type FormDebouncerFields form m = form Record (DebouncerField m)

----------
-- Core type

-- | A wrapper to represent the validation function on a form field, which can itself take
-- | the form state as its first argument. Inspired in some parts by the Validation type
-- | from purescript-polyform by @paluh.
newtype Validation form m error input output = Validation (FormFields form -> input -> m (Either error output))
derive instance newtypeValidation :: Newtype (Validation form m e i o) _
derive instance functorValidation :: Functor m => Functor (Validation form m e i)

instance applyValidation :: Monad m => Apply (Validation form m e i) where
  apply vf va = Validation \form i -> do
    vf' <- unwrap vf form i
    va' <- unwrap va form i
    pure $ vf' <*> va'

instance applicativeValidation :: Monad m => Applicative (Validation form m e i) where
  pure = Validation <<< const <<< const <<< pure <<< pure

instance altValidation :: Monad m => Alt (Validation form m e i) where
  alt v0 v1 = Validation \form i -> do
    v0' <- unwrap v0 form i
    v1' <- unwrap v1 form i
    pure $ v0' <|> v1'

instance semigroupValidation :: Semigroup (m (Either e o)) => Semigroup (Validation form m e i o) where
  append (Validation v0) (Validation v1) = Validation \form i -> v0 form i <> v1 form i

instance monoidValidation
  :: (Applicative m, Monoid (m (Either e o)), Semigroup (m (Either e o)))
  => Monoid (Validation form m e i o) where
  mempty = Validation <<< const <<< pure $ mempty

instance semigroupoidValidation :: Monad m => Semigroupoid (Validation form m e) where
  compose v1 v0 = Validation \form i -> do
    eo <- unwrap v0 form i
    either (pure <<< Left) (unwrap v1 form) eo

instance categoryValidation :: Monad m => Category (Validation form m e) where
  identity = Validation $ \_ -> pure <<< pure


----------
-- Helpers

-- | A more verbose but clearer function for running a validation function on its inputs
runValidation
  :: ∀ form m e i o
  . Monad m
  => Validation form m e i o
  -> FormFields form
  -> i -> m (Either e o)
runValidation = unwrap

-- | A function to create a record of validators that simply pass through all inputs
-- | for when no validation is needed. Provide this as your `validators` function.
noValidation
  :: ∀ form fields m vs xs
  . Monad m
  => RowToList fields xs
  => Newtype (FormValidators form m) { | vs }
  => Newtype (FormInputFields form) { | fields }
  => MapRecordWithIndex xs (ConstMapping EmptyValidators) fields vs
  => form Record InputField
  -> form Record (Validation form m)
noValidation = wrap <<< hmap EmptyValidators <<< unwrap

-- | Turn a function from (form Record FormField -> i -> o) into a proper Validation
hoistFn
  :: ∀ form m e i o
  . Monad m
  => (FormFields form -> i -> o)
  -> Validation form m e i o
hoistFn f = Validation $ \form -> pure <<< pure <<< f form

-- | Turn a function from (i -> o) into a proper Validation
hoistFn_ :: ∀ form m e i o. Monad m => (i -> o) -> Validation form m e i o
hoistFn_ f = Validation $ const $ pure <<< pure <<< f

-- | Turn a function from (form Record FormField -> i -> Either e o) into a proper Validation
hoistFnE
  :: ∀ form m e i o
  . Monad m
  => (FormFields form -> i -> Either e o)
  -> Validation form m e i o
hoistFnE f = Validation $ \form -> pure <<< f form

-- | Turn a function from (i -> Either e o) into a proper Validation
hoistFnE_ :: ∀ form m e i o. Monad m => (i -> Either e o) -> Validation form m e i o
hoistFnE_ f = Validation $ const $ pure <<< f

-- | Turn a function from (form Record FormField -> i -> m (Either e o)) into a proper Validation
hoistFnME
  :: ∀ form m e i o
  . Monad m
  => (FormFields form -> i -> m (Either e o))
  -> Validation form m e i o
hoistFnME = Validation

-- | Turn a function from (i -> m (Either e o)) into a proper Validation
hoistFnME_
  :: ∀ form m e i o
  . Monad m
  => (i -> m (Either e o))
  -> Validation form m e i o
hoistFnME_ = Validation <<< const


----------
-- Helper Types

-- | The data type used for the noValidation function's heterogenous instance
data EmptyValidators = EmptyValidators

-- | The heterogeneous instance the noValidation function
instance emptyValidators
  :: Monad m => Mapping EmptyValidators a (Validation form m e i i)
  where
    mapping EmptyValidators = const (hoistFn_ identity)
