module Form.Validation where

import Prelude

import Data.Either (Either(..))
import Data.String.Read (read)
import Data.Lens (preview)
import Data.Maybe (Maybe, maybe)
import Data.String as String
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Formless as F

-- | ToText

class ToText item where
  toText :: item -> String

instance toTextFieldError :: ToText FieldError where
  toText EmptyField = "This field is required."
  toText InvalidEmail = "That email is not valid."
  toText EmailInUse = "That email is already being used."
  toText (TooShort n) = "You must enter at least " <> show n <> " characters."
  toText (InvalidNumber str) = "Could not parse \"" <> str <> "\" to a valid integer."
  toText (NotEnoughMoney) = "You don't have that much money."

showError :: ∀ e o. ToText e => F.FormFieldResult e o -> Maybe String
showError = map toText <<< preview F._Error

-- | Type
newtype Email = Email String
derive newtype instance showEmail :: Show Email

-- | Validators

data FieldError
  = EmptyField
  | InvalidEmail
  | EmailInUse
  | TooShort Int
  | InvalidNumber String
  | NotEnoughMoney

minLength
  :: forall form m
  . Monad m
  => Int
  -> F.Validation form m FieldError String String
minLength n = F.hoistFnE_ \str ->
  let l = String.length str
  in if l >= n
    then Right str
    else Left $ TooShort n

emailFormat :: ∀ form m. Monad m => F.Validation form m FieldError String Email
emailFormat = F.hoistFnE_ $ \str ->
  if String.contains (String.Pattern "@") str
    then Right $ Email str
    else Left InvalidEmail

strIsNumber :: ∀ form m. Monad m => F.Validation form m FieldError String Number
strIsNumber = F.hoistFnE_ $ \str ->
  maybe (Left $ InvalidNumber str) Right (read str)

enoughMoney :: ∀ form m. MonadAff m => F.Validation form m FieldError Number Number
enoughMoney = F.hoistFnME_ $ \i -> do
  void <<< liftAff <<< delay $ Milliseconds 2000.0
  pure $ if (i < 1000.0)
    then Right i
    else Left NotEnoughMoney
