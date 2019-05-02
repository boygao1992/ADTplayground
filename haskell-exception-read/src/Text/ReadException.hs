module Text.ReadException where

import Import
import Data.Typeable (TypeRep, typeRep)
import RIO.ByteString (getLine)
import qualified RIO.Text as Text (unpack)

data ReadException = ReadException String TypeRep
  deriving (Typeable)

instance Show ReadException where
  show (ReadException s typ) =
    concat
    [ "Unable to parse as "
    , show typ
    , ": "
    , show s
    ]

instance Exception ReadException

readM :: forall m a. (MonadThrow m, Read a, Typeable a) => String -> m a
readM s = res
  where
    res = case readMaybe s of
      Just x -> pure x
      Nothing -> throwM $ ReadException s (typeRep res)

readLine1 :: forall m n a. (MonadIO m, MonadThrow n, Read a, Typeable a) => m (n a)
readLine1 = readM . Text.unpack . decodeUtf8Lenient <$> liftIO getLine

readLine2 :: forall m a. (MonadIO m, Read a, Typeable a) => m a
readLine2 = liftIO . join $ (readLine1 :: IO (IO a))

