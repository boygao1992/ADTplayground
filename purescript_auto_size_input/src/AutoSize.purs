module AutoSize where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Console (log) as Console
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils (classList)
import ClassNames as CN
import Styles (root) as Styles
import Halogen.HTML.CSS as HC
import Web.DOM.Element as WDE
import Web.HTML.HTMLElement as WHHE

-- | Types

type State = Unit

data Query next
  = OnInput next

type Input = Unit

type Output = Void

type IO = Aff

-- | DOM node ref
ghostRef :: H.RefLabel
ghostRef = H.RefLabel "auto-size-input-ghost"

-- | Halogen Component
render :: State -> H.ComponentHTML Query
render _ =
  HH.div [ classList [ CN.container]]
  [ HH.div [ classList [ CN.item]]
    [ HH.text "Aaliyah Schneider Jr."]
  , HH.div [ classList [ CN.item]
           , HP.ref ghostRef
           ]
    [ HH.text "Abbigail Murphy"]
  , HH.div [ classList [ CN.item]
           ]
    [ HH.text "Abe Bahringer III"]
  , HH.input [ classList [ CN.item]
             , HE.onInput $ HE.input_ OnInput
             ]
  , HC.stylesheet Styles.root
  ]

eval :: Query ~> H.ComponentDSL State Query Output IO
eval (OnInput next) = next <$ do
  mgh <- H.getHTMLElementRef ghostRef
  case mgh of
    Just gh -> do
      liTop <- H.liftEffect <<< WDE.clientWidth <<< WHHE.toElement $ gh
      H.liftEffect $ Console.log $ show liTop

    Nothing ->
      pure unit

component :: H.Component HH.HTML Query Input Output IO
component = H.component spec
  where
    spec :: H.ComponentSpec HH.HTML State Query Input Output IO
    spec =
      { initialState : const unit
      , render
      , eval
      , receiver : const Nothing
      }
