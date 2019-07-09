module Polaris.UI.Page.Home where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Polaris.UI.Block.Button as Button
import Polaris.UI.Block.Card as Card
import Polaris.UI.Block.Stack as Stack
import Polaris.UI.Block.Heading as Heading
import Polaris.UI.Block.TextContainer as TextContainer
import Polaris.UI.Block.Subheading as Subheading
import Polaris.UI.Block.List as List
import Polaris.UI.Block.ButtonGroup as ButtonGroup
import Polaris.UI.Block.Icon as Icon
import Polaris.UI.Block.Icon.Svg as IconSvg
import Polaris.UI.Block.Icon.Color as IconColor

type State = Unit

defaultInitialState :: State
defaultInitialState = unit

data Action
  = NoOp

type Query = Const Void

type Input = Unit

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. Component m
component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }

-- render :: forall m. ComponentRender m
-- render _ =
--   HH.div_
--   [ HH.h1_
--     [ HH.text "Home" ]
--   ]

render :: forall m. ComponentRender m
render _ =
  Card.card_
  [ Card.header_
    [ Stack.stack_ Stack.defaultOptions { alignment = Stack.AlignmentBaseline }
      [ Stack.stackItem_fill_
        [ Heading.heading_
          [ HH.text "Sales"
          ]
        ]
      , Stack.stackItem_
        [ Button.button_ Button.defaultOptions { buttonType = Button.Plain }
          [ Button.text_
            [ HH.text "Total Sales"
            ]
          ]
        ]
      , Stack.stackItem_
        [ Button.button_ Button.defaultOptions { buttonType = Button.Plain }
          [ Button.text_
            [ HH.text "View Sales"
            ]
          , Icon.iconSvg_ Icon.defaultOptions
            [ IconSvg.caretDownMinor
            ]
          ]
        ]
      ]
    ]
  , Card.section_
    [ TextContainer.textContainer_ TextContainer.defaultOptions
      [ HH.text "You can use sales reports to see information about your customers’ orders based on criteria such as sales over time, by channel, or by staff."
      ]
    ]
  , Card.section_
    [ Card.sectionHeader_
      [ Subheading.subheading_
        [ HH.text "Total Sales Breakdown"
        ]
      ]
    -- TODO ResourceList
    ]
  , Card.section_subdued_
    [ Card.sectionHeader_
      [ Subheading.subheading_
        [ HH.text "Deactivated reports"
        ]
      ]
    , List.list_
      [ List.item_
        [ HH.text "Payouts" ]
      , List.item_
        [ HH.text "Total Sales By Channel" ]
      ]
    ]
  , Card.section_
    [ Card.sectionHeader_
      [ Subheading.subheading_
        [ HH.text "Note"
        ]
      ]
    , TextContainer.textContainer_ TextContainer.defaultOptions
      [ HH.text "The sales reports are available only if your store is on the Shopify plan or higher."
      ]
    ]
  , Card.footer_
    [ ButtonGroup.group_ ButtonGroup.groupDefaultOptions
      [ ButtonGroup.item_ ButtonGroup.itemDefaultOptions
        [ Button.button_ Button.defaultOptions
          [ Button.text_
            [ HH.text "Dismiss"
            ]
          ]
        ]
      , ButtonGroup.item_ ButtonGroup.itemDefaultOptions
        [ Button.button_ Button.defaultOptions { buttonType = Button.Primary }
          [ Button.text_
            [ HH.text "Export Report"
            ]
          ]
        ]
      ]
    ]
  ]
