module Magento.Import.UI.Api.Category where

import Prelude

import Affjax (Response)
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.String.Read (read)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Magento.Import.Data.Categories (Categories, VerifiedCategory, stampValidity)
import Magento.Import.Data.Categories (columnName) as Categories
import Magento.Import.Data.Skus (Sku(..))
import Magento.Import.UI.Data.File (Column, RowTableHeaded(..))

parseBatchValidationResult :: BatchValidationResult -> RowTableHeaded -> Array (Tuple Sku (Array VerifiedCategory))
parseBatchValidationResult (BatchValidationResult br) (RowTableHeaded rowTable) =
  (Array.mapMaybe <@> br) \(Tuple (Sku sku) invalidCs) ->
    let
      mRawCs
        = read
        <=< Map.lookup Categories.columnName
        <=< Map.lookup sku
        $ rowTable
    in
      Tuple (Sku sku) <$> (stampValidity <$> pure invalidCs <*> mRawCs)

newtype BatchValidationResult = BatchValidationResult
  (Array (Tuple Sku Categories))
derive instance newtypeBatchValidationResult :: Newtype BatchValidationResult _
derive newtype instance decodeJsonBatchValidationResult :: DecodeJson BatchValidationResult
derive newtype instance showBatchValidationResult :: Show BatchValidationResult

batchValidate :: Column -> Aff (Response (Either String BatchValidationResult))
batchValidate col =
  (\res -> res
      { body = decodeJson <=< lmap Ajax.printResponseFormatError $ res.body }
  )
  <$>
  Ajax.post
    ResponseFormat.json
    "/category/batchValidate"
    (RequestBody.json (encodeJson col))
