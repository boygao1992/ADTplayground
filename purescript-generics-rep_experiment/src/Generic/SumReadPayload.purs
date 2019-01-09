module Generic.SumReadPayload where

import Prelude
import Type.IsEqual as Type

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), to)
import Type.Data.Symbol (SProxy)
import Type.Data.Symbol as Symbol
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool


-- data PostAction
--   = PostUpdateTitle { title :: String }
--   | PostUpdateContent { content :: String }

{-
  G.inputObjectType "PostAction" Nothing
  { postUpdateTitle :
    G.inputObjectType "PostUpdateTitle" Nothing
    { title :
      G.inputField
        (G.nonnull G.string) Nothing
    }
  , postUpdateContent :
    G.inputObjectType "PostUpdateContent" Nothing
    { content :
      G.inputField
        (G.nonnull G.string) Nothing
    }
  }

  toConstructor :: Generic PostAction rep =>
    { postUpdateTitle :: Maybe { title :: String }
    , postUpdateContent :: Maybe { content :: String }
    } -> Either String PostAction
  toConstructor inputObject = case Row.uncons inputObject of
    Nothing ->
      Left "invalid inputObject"
    Just name value restInputObject -> case value of
      Nothing ->
        toConstructor restInputObject
      Just payload ->
        Right $ SumReadPayload name value
-}

sumReadPayload :: forall a rep name payload. Generic a rep => GenericSumReadPayload name payload rep => SProxy name -> payload -> Either String a
sumReadPayload pname rec = to <$> genericSumReadPayload pname rec

class GenericSumReadPayload (name :: Symbol) payload rep where
  genericSumReadPayload :: SProxy name -> payload -> Either String rep

instance genericSumReadPayloadBaseCase ::
  ( Symbol.Equals name1 name2 b1
  , Type.IsEqual payload1 payload2 b2
  , GenericSumReadPayloadDispatch b1 b2 payload1 (Constructor name2 (Argument payload2))
  ) => GenericSumReadPayload name1 payload1 (Constructor name2 (Argument payload2)) where
    genericSumReadPayload _ rec = genericSumReadPayloadDispatch (BProxy :: BProxy b1) (BProxy :: BProxy b2) rec
else instance genericSumReadPayloadInductionStep ::
  ( GenericSumReadPayload name payload l
  , GenericSumReadPayload name payload r
  ) => GenericSumReadPayload name payload (Sum l r)
  where
    genericSumReadPayload pname rec
        = Inl <$> genericSumReadPayload pname rec
      <|> Inr <$> genericSumReadPayload pname rec

class GenericSumReadPayloadDispatch (b1 :: Bool.Boolean) (b2 :: Bool.Boolean) payload rep where
  genericSumReadPayloadDispatch :: BProxy b1 -> BProxy b2 -> payload -> Either String rep

instance genericSumReadPayloadDispatchIsEqual ::
  GenericSumReadPayloadDispatch Bool.True Bool.True payload (Constructor name (Argument payload))
  where
    genericSumReadPayloadDispatch _ _ rec = Right $ Constructor (Argument rec)
else instance genericSumReadPayloadDispatchNotEqual ::
  GenericSumReadPayloadDispatch b1 b2 payload1 (Constructor name (Argument payload2))
  where
    genericSumReadPayloadDispatch _ _ rec = Left "doesn't match"
