module Data.HObject.Record where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), either)
import Data.HObject (HObject)
import Effect.Exception (Error, error)
import Foreign (unsafeToForeign)
import Foreign.Generic (defaultOptions)
import Foreign.Generic.Class (class Decode_, decode_)
import Prelude (($), show)


-- | Converts a Json to a Record
                  -- TODO DecodeRecord
jsonToRecord :: forall a. Decode_ a => Json -> Either Error a
jsonToRecord json = either (\err -> Left (error $ show err)) Right $ runExcept (decode_ defaultOptions (unsafeToForeign json))


-- | Converts an HObject to a Record
                    -- TODO DecodeRecord
hObjToRecord :: forall a b. Decode_ b => HObject a -> Either Error b
hObjToRecord hobj = either (\err -> Left (error $ show err)) Right $ runExcept (decode_ defaultOptions (unsafeToForeign hobj))


-- | Gets the contructor name for the type
foreign import structName :: forall a . a -> String
