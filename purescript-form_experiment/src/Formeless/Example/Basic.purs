module Formless.Example.Basic where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Formless.Validation (Validation, hoistFnE_, hoistFnME_)
import Data.Variant (Variant, inj)
import Type.Data.Symbol (SProxy(..))

-- | Types

data FieldError
  = InvalidEmail
  | TooShort Int
  | TooLong Int
  | EmptyField
  | InvalidInt String
  | EmailInUse

type FormValidationSync i o =
  forall form m.
  Monad m => Validation form m FieldError i o

type FormValidationAsync i o =
  forall form m.
  MonadAff m => Validation form m FieldError i o

newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive newtype instance eqEmail :: Eq Email
derive newtype instance ordEmail :: Ord Email

-- | Synchronous Validators

emailFormat :: FormValidationSync String Email
emailFormat = hoistFnE_ \str ->
  if not <<< String.contains (Pattern "@") $ str
  then Left InvalidEmail
  else Right <<< Email $ str

minLength :: Int -> FormValidationSync String String
minLength min = hoistFnE_ \str ->
  if String.length str < min
  then Left <<< TooShort $ min
  else Right str

maxLength :: Int -> FormValidationSync String String
maxLength max = hoistFnE_ \str ->
  if String.length str > max
  then Left <<< TooLong $ max
  else Right str

exists :: forall a. FormValidationSync (Maybe a) a
exists = hoistFnE_ $ maybe (Left EmptyField) Right

strIsInt :: FormValidationSync String Int
strIsInt = hoistFnE_ \str ->
      maybe (Left <<< InvalidInt $ str) Right
  <<< Int.fromString
    $ str

nonEmptyArray :: forall a. FormValidationSync (Array a) (Array a)
nonEmptyArray = hoistFnE_ \arr ->
  if Array.length arr <= 0
  then Left EmptyField
  else Right arr

nonEmptyStr :: FormValidationSync String String
nonEmptyStr = minLength 1

-- | Asynchronous Validators

emailIsUsed :: FormValidationAsync Email Email
emailIsUsed = hoistFnME_ \e@(Email email) -> do
  _ <- liftAff $ delay $ Milliseconds 1000.0
  pure $
    if not <<< String.contains (Pattern "t") $ email
    then Left EmailInUse
    else Right e

-- enoughMoney :: forall form m open. MonadAff m => Validation form m (Variant (notEnoughMoney :: Unit | open )) Int Int
-- enoughMoney = hoistFnME_ \amount -> do
--   pure $
--     if (amount > 1000)
--     then Left $ inj (SProxy :: SProxy "notEnoughMoney") unit
--     then Right amount
