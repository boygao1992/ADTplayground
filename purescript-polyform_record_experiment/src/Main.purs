module Main where

import Prelude

import Data.Array (any, elem)
import Data.Bifunctor (lmap)
import Data.String (Pattern(..), contains, length)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Random (random)
import Form.Validation (V(..), Validation(..), runValidation)
import Form.Validation as Validation
import Prim.RowList (kind RowList)
import Type.Prelude (RProxy(..), SProxy)
import Type.Row (type (+))
import Type.Row.MkLabels (mkLabels)

-- | Types

type Input err value = V (Array err) value

-- | Email Validator

type EmailFormat r =
  ( emailFormat :: String
  | r
  )
type EmailIsUsed r =
  ( emailIsUsed :: String
  | r
  )
type EmailErrors r =
  ( EmailFormat
  + EmailIsUsed
  + r
  )
_emailErrors ::
  { emailFormat :: SProxy "emailFormat"
  , emailIsUsed :: SProxy "emailIsUsed"
  }
_emailErrors = mkLabels (RProxy ::RProxy (EmailErrors ()))

displayEmailError
  :: forall r
   . (Variant r -> String)
  -> Variant (EmailErrors r)
  -> String
displayEmailError =
  Variant.onMatch
  { emailFormat: \email -> "Invalid email format: `" <> email <> "`"
  , emailIsUsed: \email -> "Email is used: `" <> email <> "`"
  }

emailFormat :: forall m r.
  Monad m =>
    Validation m
      (Array
        (Variant (EmailFormat r))
      )
      String
      String
emailFormat = Validation.hoistFnV \e →
  if contains (Pattern "@") e
    then pure e
    else Invalid [Variant.inj _emailErrors.emailFormat e]

emailIsUsed :: forall r.
  Validation Effect
    (Array
       (Variant (EmailIsUsed r))
    )
    String
    String
emailIsUsed = Validation.hoistFnMV \e → do
  v ← random
  pure $ if v > 0.5
    then Invalid [Variant.inj _emailErrors.emailIsUsed e]
    else pure e

-- | Password Validator

type HasDigit r =
  ( hasDigit ∷ String
  | r
  )
type MaxLength r =
  ( maxLength ∷ Tuple Int String
  | r
  )
type MinLength r =
  ( minLength ∷ Tuple Int String
  | r
  )
type PasswordErrors r =
  ( HasDigit
  + MaxLength
  + MinLength
  + r
  )
_passwordErrors ::
  { hasDigit :: SProxy "hasDigit"
  , maxLength :: SProxy "maxLength"
  , minLength :: SProxy "minLength"
  }
_passwordErrors = mkLabels (RProxy :: RProxy (PasswordErrors ()))

displayPasswordError
  :: forall r
   . (Variant r -> String)
  -> Variant (PasswordErrors r)
  -> String
displayPasswordError =
  Variant.onMatch
  { hasDigit:
      \str -> "`"<> str <> "` doesn't have digit."
  , maxLength:
      \(Tuple l str) -> "`"<> str <> "` exceeds the maximum length " <> show l
  , minLength:
      \(Tuple l str) -> "`"<> str <> "` doesn't meet the minimum length " <> show l
  }

minLength :: forall m r.
  Monad m => Int ->
    Validation m
      (Array
          (Variant (MinLength r))
      )
      String
      String
minLength m = Validation.hoistFnV \p →
  if length p < m
    then Invalid [Variant.inj _passwordErrors.minLength (Tuple m p)]
    else pure p

maxLength :: forall m r.
  Monad m => Int ->
    Validation m
      (Array
          (Variant (MaxLength r))
      )
      String
      String
maxLength m = Validation.hoistFnV \p →
  if length p > m
    then Invalid [Variant.inj _passwordErrors.maxLength (Tuple m p)]
    else pure p

hasDigit :: forall m r.
  Monad m =>
    Validation m
      (Array
        (Variant (HasDigit r))
      )
      String
      String
hasDigit = Validation.hoistFnV \p →
  let
    chars = toCharArray p
  in
    if any (_ `elem` chars) (toCharArray "0123456789")
    then pure p
    else Invalid [Variant.inj _passwordErrors.hasDigit p]

-- | Field

data Field
  = EmailField
      ( Input
        (Variant (EmailErrors ()))
        String
      )
  | PasswordField
      ( Input
        (Variant (PasswordErrors ()))
        String
      )
instance showField :: Show Field where
  show (EmailField v) =
    show $ lmap (map (Variant.case_ # displayEmailError )) v
  show (PasswordField v) =
    show $ lmap (map (Variant.case_ # displayPasswordError)) v

emailFieldValidation :: forall r.
  Validation Effect
    (Array
     (Variant (EmailErrors r))
    )
    String
    String
emailFieldValidation = emailFormat >>> emailIsUsed

passwordFieldValidation :: forall m r.
  Monad m => Int -> Int ->
    Validation m
      (Array
        (Variant (PasswordErrors r))
      )
      String
      String
passwordFieldValidation min max = maxLength max *> minLength min *> hasDigit

-- | Form

type Form = Tuple (Array String) (Array Field)

fieldForm
  :: forall o s m i e
   . Monad m
  => (s -> i)
  -> (V e o -> Field)
  -> Validation m e i o
  -> Validation m Form s o
fieldForm fetchValue constructor fieldValidation =
  Validation $ \inputRecord → do
    let inputValue = fetchValue inputRecord
    r ← Validation.runValidation fieldValidation inputValue
    pure $ case r of
      valid@Valid e v → Valid (embed <<< constructor $ valid) v
      invalid@Invalid e → Invalid (embed <<< constructor $ invalid)
  where
    embed :: Field -> Form
    embed field = Tuple [] [field]

emailForm :: forall r.
  Validation Effect Form
    { email :: String
    | r
    }
    String
emailForm = fieldForm (_.email) EmailField emailFieldValidation

buildPasswordForm
  :: forall m s
   . Monad m
  => (s -> String)
  -> Validation m Form s String
buildPasswordForm fetch =
  fieldForm fetch PasswordField (passwordFieldValidation 5 50)

passwordForm :: forall m r.
  Monad m =>
  Validation m Form
    { password1 :: String
    , password2 :: String
    | r
    }
    String
passwordForm
  = (   {password1: _, password2: _}
    <$> (buildPasswordForm _.password1)
    <*> (buildPasswordForm _.password2)
    )
  >>> Validation.hoistFnV \({ password1, password2 }) →
    if password1 /= password2
      then Invalid (Tuple ["Password1 and Password2 don't match"] [])
      else pure password1

signupForm :: forall r.
  Validation Effect Form
    { password1 :: String
    , password2 :: String
    , email :: String
    | r
    }
    { password :: String
    , email :: String
    }
signupForm = {password: _, email: _} <$> passwordForm <*> emailForm

printResult ::
  V Form
    { password :: String
    , email :: String
    }
 -> Effect Unit
printResult =
  case _ of
    Valid form value → do
      log "FORM VALID:"
      logShow form
      log "- Final value:"
      logShow value

    Invalid (Tuple result1 result2) → do
      log "FORM INVALID:"
      log "- Global results:"
      logShow result1
      log "- Local results:"
      logShow result2

main :: Effect Unit
main = do
  log "EXAMPLE"

  v1 ← runValidation signupForm
        { email: "wrongemailformat"
        , password1: "shrt"
        , password2: "nodigits"
        }
  printResult v1

  log "\n\n"

  v2 ← runValidation signupForm
        { email: "email@example.com"
        , password1: "password1"
        , password2: "password2"
        }
  printResult v2

  log "\n\n"

  v3 ← runValidation signupForm
        { email: "email@example.com"
        , password1: "password921"
        , password2: "password921"
        }
  printResult v3
