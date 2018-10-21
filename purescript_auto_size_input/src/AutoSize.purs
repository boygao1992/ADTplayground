module AutoSize where

import Prelude

import ClassNames as CN
import Data.Array (snoc)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
-- import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils (classList)
import Styles (root, inputWidth) as Styles
import Web.DOM.Element as WDE
-- import Web.Event.Event as WEE
import Web.HTML.HTMLElement (focus, toElement) as WHHE
import Web.UIEvent.KeyboardEvent as KE

-- | Types

type State =
  { input :: InputState
  , selections :: Array String
  }

type InputState =
  { text :: String
  , width :: Number
  }

data Query next
  = OnInput String next
  | OnKeyDown KE.KeyboardEvent next

type Input = Unit

type Output = Void

type IO = Aff

-- | Constants

emptyInputState :: InputState
emptyInputState =
  { text : ""
  , width : 0.0
  }

initialState :: State
initialState =
  { input : emptyInputState
  , selections : [ "Ada Hirthe", "Abbigail Murphy", "Aaliyah Schneider Jr." ]
  }

-- | DOM node ref
inputRef :: H.RefLabel
inputRef = H.RefLabel "auto-size-input"
ghostRef :: H.RefLabel
ghostRef = H.RefLabel "auto-size-input-ghost"

-- | Halogen Component
render :: State -> H.ComponentHTML Query
render { input, selections } =
  HH.div [ classList [ CN.container]]
  ( renderList selections
  `snoc` renderAutoSizeInput input
  `snoc` HC.stylesheet Styles.root
  )

  where
    renderItem :: forall q. String -> H.ComponentHTML q
    renderItem str =
      HH.div [ classList [ CN.item] ]
      [ HH.text str ]

    renderList :: forall q. Array String -> Array (H.ComponentHTML q)
    renderList = map renderItem

    renderAutoSizeInput :: InputState -> H.ComponentHTML Query
    renderAutoSizeInput { text, width } =
      HH.div [ classList [ CN.item]]
      [ HH.input [ HE.onValueInput $ HE.input OnInput
                 , HE.onKeyDown $ HE.input OnKeyDown
                 , HC.style $ Styles.inputWidth width
                 , HP.ref inputRef
                 ]
      , HH.div [ classList [ CN.ghostItem ]
                , HP.ref ghostRef]
        [ HH.text text]
      ]


eval :: Query ~> H.ComponentDSL State Query Output IO
eval (OnInput inputText next) = next <$ do
  H.modify_ $ \st -> st { input = st.input { text = inputText } }
  mgh <- H.getHTMLElementRef ghostRef
  case mgh of
    Just gh -> do
      textWidth <- H.liftEffect <<< WDE.clientWidth <<< WHHE.toElement $ gh
      H.modify_ $ \st -> st { input = st.input { width = textWidth }}

    Nothing ->
      pure unit
eval (OnKeyDown ev next) = next <$ do
  case KE.key ev of
    "Enter" -> do
      H.modify_ $ \st -> st { input = emptyInputState
                            , selections = st.selections `snoc` st.input.text
                            }
      ie <- H.getHTMLElementRef inputRef
      traverse_ (H.liftEffect <<< WHHE.focus) ie
    _ ->
      pure unit

component :: H.Component HH.HTML Query Input Output IO
component = H.component spec
  where
    spec :: H.ComponentSpec HH.HTML State Query Input Output IO
    spec =
      { initialState : const initialState
      , render
      , eval
      , receiver : const Nothing
      }
