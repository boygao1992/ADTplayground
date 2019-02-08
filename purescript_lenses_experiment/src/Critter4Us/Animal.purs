module Critter4Us.Animal where

import Data.Lens (Lens', lens, set, over)
import Data.Array (snoc)

-- | Model
type Id = Int
type Name = String
type Tag = String

type Animal =
  { id :: Id
  , name :: String
  , tags :: Array Tag
  }

-- | Lenses
_tags :: Lens' Animal (Array Tag)
_tags = lens _.tags (_ { tags = _ })

-- | API
named :: Id -> Name -> Animal
named id name = { id, name, tags: [] }

clearTags :: Animal -> Animal
clearTags = set _tags []

addTag :: Tag -> Animal -> Animal
addTag tag = over _tags (_ `snoc` tag)
