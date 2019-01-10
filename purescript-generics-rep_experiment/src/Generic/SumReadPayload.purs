module Generic.SumReadPayload where

import Prelude
import Type.IsEqual as Type

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), to)
import Data.Maybe (Maybe(..))
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Row (RProxy(..))
import Type.Row as Row
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Record as Record


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


toConstructor :: forall a rep record. Generic a rep => GenericToConstructor record rep => record -> Either String a
toConstructor rec = to <$> genericToConstructor rec

class GenericToConstructor record rep where
  genericToConstructor :: record -> Either String rep

instance genericToConstructorFirstJustBaseCase ::
  ( Symbol.IsSymbol name
  , Row.Cons name (Maybe typ) restRow row
  ) => GenericToConstructor (Record row) (Constructor name (Argument typ))
  where
    genericToConstructor rec = case Record.get (SProxy :: SProxy name) rec of
      Nothing ->
        Left "None of the fields has payload"
      Just arg ->
        Right $ Constructor (Argument arg)
else instance genericToConstructorFirstJustInductionStep ::
  ( Symbol.IsSymbol name
  , Row.Cons name (Maybe typ) restRow row
  , GenericToConstructor (Record row) r
  , GenericToConstructorRestNoJust (Record row) r
  ) => GenericToConstructor (Record row) (Sum (Constructor name (Argument typ)) r)
  where
    genericToConstructor rec = case Record.get (SProxy :: SProxy name) rec of
      Nothing ->
        Inr <$> genericToConstructor rec
      Just arg ->
        if genericToConstructorRestNoJust rec (Proxy :: Proxy r)
        then
          Right $ Inl $ Constructor $ Argument arg
        else
          Left "More than one fields have payloads"

class GenericToConstructorRestNoJust record rep where
  genericToConstructorRestNoJust :: record -> Proxy rep -> Boolean

instance genericToConstructorRestNoJustBaseCase ::
  ( Symbol.IsSymbol name
  , Row.Cons name (Maybe typ) restRow row
  ) => GenericToConstructorRestNoJust (Record row) (Constructor name (Argument typ))
  where
    genericToConstructorRestNoJust rec _ = case Record.get (SProxy :: SProxy name) rec of
      Nothing -> true
      Just _ -> false
else instance genericToConstructorRestNoJustInductionStep ::
  ( GenericToConstructorRestNoJust record l
  , GenericToConstructorRestNoJust record r
  ) => GenericToConstructorRestNoJust record (Sum l r)
  where
    genericToConstructorRestNoJust rec _
        = genericToConstructorRestNoJust rec (Proxy :: Proxy l)
       && genericToConstructorRestNoJust rec (Proxy :: Proxy r)


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
