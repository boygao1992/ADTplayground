-- | Not an entire component, but pieces that only make sense within
-- | the context of components that meet the constraints on these
-- | functions.
module Ocelot.Part.Modal where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css, (<&>))
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHH
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

----------
-- Eval Partials

-- A function that will register an event source on the window
initializeWith
  :: ∀ s act ps o m
   . MonadAff m
  => (KE.KeyboardEvent -> Maybe act)
  -> H.HalogenM s act ps o m H.SubscriptionId
initializeWith toAction = do
  document <- H.liftEffect $ WHW.document =<< WH.window
  H.subscribe
    $ ES.eventListenerEventSource
      KET.keydown
      (WHH.toEventTarget document)
      (toAction <=< KE.fromEvent)

whenClose
  :: ∀ s act ps o m
  . MonadAff m
  => KE.KeyboardEvent
  -> H.SubscriptionId
  -> H.HalogenM s act ps o m Unit
  -> H.HalogenM s act ps o m Unit
whenClose ev sid close =
  when (KE.code ev == "Escape") do
    H.unsubscribe sid
    close

----------
-- Render Partials

-- Modals already come with the ClickOutside event baked in, while
-- end users are responsible for handling it somehow.
modal
  :: ∀ act ps m
  . act
  -> Array (HH.IProp HTMLdiv act)
  -> Array (H.ComponentHTML act ps m)
  -> H.ComponentHTML act ps m
modal click iprops html =
  HH.div_
    [ HH.div [ HP.classes backgroundClasses
             , HE.onClick $ Just <<< const click
             ]
      [ HH.div
        ( [ HP.classes modalClasses ] <&> iprops )
        html
      ]
    ]

modal_
  :: ∀ act ps m
  . act
  -> Array (H.ComponentHTML act ps m)
  -> H.ComponentHTML act ps m
modal_ query = modal query []


-----------
-- Blocks

type HeaderProps p i =
  { buttons :: Array (HH.HTML p i)
  , title :: Array (HH.HTML p i)
  }

backgroundClasses :: Array HH.ClassName
backgroundClasses = HH.ClassName <$>
  [ "fixed"
  , "pin"
  , "bg-black-modal-a90"
  , "fade-in"
  , "z-60" -- NOTE
  ]

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "fixed"
  , "pin-x"
  , "pin-t"
  , "my-20"
  , "m-auto"
  , "max-w-lg"
  , "slide-down"
  , "relative" -- NOTE
  ]

bodyClasses :: Array HH.ClassName
bodyClasses = HH.ClassName <$>
  [ "relative"
  , "bg-grey-95"
  , "overflow-auto"
  , "max-h-full"
  , "w-full"
  , "flex-col"
  , "flex"
  , "rounded-b"
  ]

body
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
body iprops html =
  HH.div
    ( [ HP.classes bodyClasses ] <&> iprops )
    html

body_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
body_ = body []

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "h-24"
  , "flex"
  ]

outerHeaderClasses :: Array HH.ClassName
outerHeaderClasses = HH.ClassName <$>
  [ "bg-white"
  , "w-full"
  , "px-6"
  , "items-center"
  , "flex"
  , "rounded-t"
  ]

innerHeaderClasses :: Array HH.ClassName
innerHeaderClasses = HH.ClassName <$>
  [ "w-full"
  , "items-center"
  , "mx-auto"
  , "flex"
  ]

header
  :: ∀ p i
   . HeaderProps p i
  -> HH.HTML p i
header props =
  HH.div
    [ HP.classes headerClasses ]
    [ HH.header
      [ HP.classes outerHeaderClasses ]
      ( [ HH.div
        [ HP.classes innerHeaderClasses ]
          [ Format.subHeading
            [ css "mb-0" ]
            props.title
          ]
        ]
        <> props.buttons
      )
    ]