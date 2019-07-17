{-# LANGUAGE TemplateHaskell #-}
module Run (run) where

import Import
import Prelude (showsPrec)

import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Dependent.Sum
import qualified Data.Dependent.Map as DMap

-- DSum example

data PersonT a where
  PersonName :: PersonT Text
  PersonAge :: PersonT Int
  PersonSpecialField :: Text -> PersonT Text
  -- PersonHole :: a -> PersonT a

$(deriveGEq ''PersonT)
-- instance GEq PersonT where
--   geq PersonName PersonName = Just Refl
--   geq PersonAge PersonAge = Just Refl
--   geq (PersonSpecialField x) (PersonSpecialField y) =
--     if x == y
--     then Just Refl
--     else Nothing
--   -- geq (PersonHole (_ :: a)) (PersonHole (_ :: b)) = Nothing -- NOTE `a` and `b` cannot be unified from term level
--   geq _ _ = Nothing

instance Eq (PersonT a) where
  (==) = defaultEq

$(deriveGCompare ''PersonT)
-- instance GCompare PersonT where -- NOTE from Bottom to Top
--   gcompare PersonName PersonName = GEQ
--   gcompare PersonName _ = GLT
--   gcompare _ PersonName = GGT

--   gcompare PersonAge PersonAge = GEQ
--   gcompare PersonAge _ = GLT
--   gcompare _ PersonAge = GGT

--   -- NOTE priority: type-level gcompare > term-level compare
--   gcompare (PersonSpecialField x) (PersonSpecialField y) = case compare x y of
--     EQ -> GEQ
--     LT -> GLT
--     GT -> GGT
--   -- gcompare (PersonSpecialField _) _ = GLT
--   -- gcompare _ (PersonSpecialField _) = GGT

--   -- gcompare (PersonHole (_ :: a)) (PersonHole (_ :: b)) = GLT -- NOTE randomly picked

$(deriveGShow ''PersonT)
-- instance Show (PersonT a) where
--   showsPrec _ PersonName = showString "name"
--   showsPrec _ PersonAge = showString "age"
--   showsPrec _ (PersonSpecialField _) = showString "special_field"

-- instance GShow PersonT where
--   gshowsPrec = showsPrec

instance ShowTag PersonT Identity where
  showTaggedPrec PersonName = showsPrec
  showTaggedPrec PersonAge = showsPrec
  showTaggedPrec (PersonSpecialField _) = showsPrec

-- GADT playground

displayPersonTText :: PersonT Text -> Text
displayPersonTText PersonName = "PersonName"
displayPersonTText (PersonSpecialField s) = s
-- displayPersonTText (PersonHole s) = s -- NOTE a ~ Text

displayPersonT :: PersonT a -> Text
displayPersonT PersonName = "PersonName"
displayPersonT PersonAge = "PersonAge"
displayPersonT (PersonSpecialField s) = s
-- displayPersonT (PersonHole a) = fromString $ show a -- NOTE Show a => a

-- DSum smart constructors
type PersonField = DSum PersonT Identity
name :: Text -> PersonField
name = (==>) PersonName

age :: Int -> PersonField
age = (==>) PersonAge

specialField :: Text -> PersonField
specialField = (==>) (PersonSpecialField "special key")

-- hole :: a -> PersonField
-- hole a = (PersonHole a) ==> a -- NOTE redundant `a` stored in tag

-- DMap example

dmap1 :: DMap.DMap PersonT Identity
dmap1 = DMap.fromList
  [ name "wenbo"
  , age 14
  , specialField "special value"
  -- , hole (1.2 :: Double) -- NOTE its consumer function needs to work for all types not just Double
  ]


run :: RIO App ()
run = do
  logInfo "We're inside the application!"

  logInfo $ displayShow $ dmap1
