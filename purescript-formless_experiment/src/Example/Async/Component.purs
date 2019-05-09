module Example.Async.Component where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Form.UI as UI
import Form.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = Formless (F.Message' DonateForm) a

type ChildQuery = F.Query' DonateForm Aff
type ChildSlot = Unit

type State = Unit
type Input = Unit
type Output = Void
type IO = Aff

render :: Unit -> H.ParentHTML Query ChildQuery ChildSlot IO
render _ =
  UI.section_
  [ UI.h1_ [ HH.text "Formless" ]
  , UI.h2_ [ HH.text "A form with debounced async fields." ]
  , UI.p_ $
      "If you have fields with expensive validation, you can debounce modifications to the field "
      <> "with the async versions of setValidate and modifyValidate query functions. The result "
      <> "type of the form field lets you know whether the field has not been validated, is "
      <> "currently validating, or has produced an error or result."
  , HH.br_
  , HH.slot unit
      F.component { initialInputs, validators, render: renderForm }
      (HE.input Formless)
  ]

eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output IO
eval (Formless (F.Submitted formOutputs) a) = a <$ do
  let result = F.unwrapOutputFields formOutputs
  H.liftEffect $ logShow result
eval (Formless _ a) = pure a

component :: H.Component HH.HTML Query Input Output Aff
component = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }

-- | Formless

newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive newtype instance showName :: Show Name
newtype Balance = Balance Number
derive instance newtypeBalance :: Newtype Balance _
derive newtype instance showBalance :: Show Balance

newtype DonateForm r f = DonateForm
  (r ( name    :: f V.FieldError String Name
     , email   :: f V.FieldError String V.Email
     , balance :: f V.FieldError String Balance
     )
  )
derive instance newtypeDonateForm :: Newtype (DonateForm r f) _

prx :: F.SProxies DonateForm
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy DonateForm

initialInputs :: DonateForm Record F.InputField
initialInputs = F.mkInputFields $ F.FormProxy :: F.FormProxy DonateForm

validators
  :: forall m
  . MonadAff m
  => DonateForm Record (F.Validation DonateForm m)
validators = DonateForm
  { name: V.minLength 5 >>> F.hoistFn_ wrap
  , email: V.emailFormat
  , balance: V.strIsNumber >>> V.enoughMoney >>> F.hoistFn_ wrap
  }

renderForm :: F.State DonateForm Aff -> F.HTML' DonateForm IO
renderForm { form } =
  UI.formContent_
  [ UI.input
      { label: "Name"
      , help: UI.resultToHelp "Write your name" $ F.getResult prx.name form
      , placeholder: "Frank Ocean"
      }
      [ HP.value $ F.getInput prx.name form
      , HE.onValueInput $ HE.input $ F.setValidate prx.name
      ]
  , UI.input
      { label: "Email"
      , help: UI.resultToHelp "Provide your email address" $ F.getResult prx.email form
      , placeholder: "john@hamm.com"
      }
      [ HP.value $ F.getInput prx.email form
      , HE.onValueInput $ HE.input $ F.asyncSetValidate (Milliseconds 300.0) prx.email
      ]
  , UI.input
      { label: "Donation"
      , help: UI.resultToHelp "How many dollas do you want to spend?" $ F.getResult prx.balance form
      , placeholder: "1000"
      }
      [ HP.value $ F.getInput prx.balance form
      , HE.onValueInput <<< HE.input
        $ F.asyncSetValidate (Milliseconds 1000.0) prx.balance
      ]
  , UI.buttonPrimary
      [ HE.onClick $ HE.input_ F.submit ]
      [ HH.text "Submit" ]
  ]
