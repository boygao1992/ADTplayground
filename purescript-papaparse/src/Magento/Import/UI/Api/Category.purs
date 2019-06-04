module Magento.Import.UI.Api.Category where

import Prelude

import Affjax (Response)
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Magento.Import.UI.Data.File (Column)
import Data.Bifunctor (lmap)


newtype BatchValidationResult = BatchValidationResult
  (Array (Tuple String (Array String)))
derive newtype instance decodeJsonBatchValidationResult
  :: DecodeJson BatchValidationResult
derive newtype instance showBatchValidationResult :: Show BatchValidationResult

batchValidate :: Column -> Aff (Response (Either String BatchValidationResult))
batchValidate col =
  (\res -> res
      { body = lmap Ajax.printResponseFormatError res.body >>= decodeJson }
  )
  <$>
  Ajax.post
    ResponseFormat.json
    "/category/batchValidate"
    (RequestBody.json (encodeJson col))
