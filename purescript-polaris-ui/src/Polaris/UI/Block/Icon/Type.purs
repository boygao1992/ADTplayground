module Polaris.UI.Block.Icon.Type where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data IconType
  = Add
  | Alert
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowUpDown
  | Calendar
  | Cancel
  | CancelSmall
  | CaretDown
  | CaretUp
  | Checkmark
  | ChevronDown
  | ChevronLeft
  | ChevronRight
  | ChevronUp
  | CircleCancel
  | CircleChevronDown
  | CircleChevronLeft
  | CircleChevronRight
  | CircleChevronUp
  | CircleInformation
  | CirclePlus
  | CirclePlusOutline
  | Conversation
  | Delete
  | Disable
  | Dispute
  | Duplicate
  | Embed
  | Export
  | External
  | Help
  | Home
  | HorizontalDots
  | Import
  | LogOut
  | Menu
  | Notes
  | Notification
  | OnlineStore
  | Orders
  | Print
  | Products
  | Profile
  | Refresh
  | Risk
  | Save
  | Search
  | Subtract
  | View
derive instance genericIconType :: Generic IconType _
derive instance eqIconType :: Eq IconType
derive instance ordIconType :: Ord IconType
instance showIconType :: Show IconType where
  show x = genericShow x
