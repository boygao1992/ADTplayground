module Tonatona.Servant.Options where

import RIO
import qualified RIO.List as List (intercalate)
import Data.ByteString.Char8 (unpack, split)
import Text.Read (readsPrec)
import Network.Wai.Handler.Warp (HostPreference)
import Options.Applicative (strOption, option, auto, long, metavar, value, help, flag, showDefault)

import Tonatona.Options.Parser (HasParser, parser)

class HasServantOptions options where
  servantOptionsL :: Lens' options ServantOptions

data ServantOptions = ServantOptions
  { host :: !Host
  , port :: !Port
  , logging :: !Logging
  , gzip :: !Gzip
  , protocol :: !Protocol
  , tls :: !ServantTlsOptions
  } deriving (Eq, Show)
instance HasParser ServantOptions where
  parser = ServantOptions <$> parser <*> parser <*> parser <*> parser <*> parser <*> parser
instance HasServantOptions ServantOptions where
  servantOptionsL = id

newtype Host = Host { unHost :: HostPreference }
  deriving newtype (Eq, Ord, IsString, Read, Show)
_host :: Lens' ServantOptions HostPreference
_host = lens (unHost . host) (\x y -> x { host = Host y })
instance HasParser Host where
  parser =
    strOption
    $ long "sh"
    <> metavar "HOST"
    <> value "127.0.0.1"
    <> showDefault
    <> help "set Servant Host"

newtype Port = Port { unPort :: Int }
  deriving newtype (Eq, Ord, Read, Show)
_port :: Lens' ServantOptions Int
_port = lens (unPort . port) (\x y -> x { port = Port y })
instance HasParser Port where
  parser =
    option auto
    $ long "sP"
    <> metavar "PORT"
    <> value (Port 8000)
    <> showDefault
    <> help "set Servant Port"

newtype Logging = Logging { unLogging :: Bool }
  deriving newtype (Eq, Ord, Read, Show)
_logging :: Lens' ServantOptions Bool
_logging = lens (unLogging . logging) (\x y -> x { logging = Logging y })
instance HasParser Logging where
  parser = Logging <$>
    flag True False
    ( long "sL"
    <> help "disable Servant Request Logger Middleware"
    )

newtype Gzip = Gzip { unGzip :: Bool }
  deriving newtype (Eq, Ord, Read, Show)
_gzip :: Lens' ServantOptions Bool
_gzip = lens (unGzip . gzip) (\x y -> x { gzip = Gzip y })
instance HasParser Gzip where
  parser = Gzip <$>
    flag True False
    ( long "sG"
    <> help "disable Servant Gzip Middleware"
    )

data Protocol
  = Http
  | Https
  deriving (Eq, Ord, Read, Show, Enum)
_protocol :: Lens' ServantOptions Protocol
_protocol = lens protocol (\x y -> x { protocol = y })
instance HasParser Protocol where
  parser =
    flag Http Https
    $ long "sp"
    <> help "set Servant Protocol to Https (default: Http)"

data ServantTlsOptions = ServantTlsOptions
  { certFilePath :: !CertFilePath
  , certKeyFilePath :: !CertKeyFilePath
  , certChainFilePaths :: !CertChainFilePaths
  }
  deriving (Eq, Show)
_tls :: Lens' ServantOptions ServantTlsOptions
_tls = lens tls \x y -> x { tls = y }
instance HasParser ServantTlsOptions where
  parser = ServantTlsOptions <$> parser <*> parser <*> parser

newtype CertFilePath = CertFilePath { unCertFilePath :: FilePath }
  deriving newtype (Eq, Ord, Read, Show, IsString)
_tlsCertFilePath :: Lens' ServantOptions FilePath
_tlsCertFilePath = _tls . (lens (unCertFilePath . certFilePath) (\x y -> x { certFilePath = CertFilePath y}))
instance HasParser CertFilePath where
  parser =
    strOption
    $ long "sTc"
    <> metavar "Cert File"
    <> value ""
    <> help "set Servant TLS Certificate File Path"

newtype CertKeyFilePath = CertKeyFilePath { unCertKeyFilePath :: FilePath }
  deriving newtype (Eq, Ord, Read, Show, IsString)
_tlsCertKeyFilePath :: Lens' ServantOptions FilePath
_tlsCertKeyFilePath = _tls . (lens (unCertKeyFilePath . certKeyFilePath) (\x y -> x { certKeyFilePath = CertKeyFilePath y}))
instance HasParser CertKeyFilePath where
  parser =
    strOption
    $ long "sTck"
    <> metavar "Key File"
    <> value ""
    <> help "set Servant TLS Certificate Key File Path"

newtype CertChainFilePath = CertChainFilePath
  { unCertChainFilePath :: FilePath }
  deriving newtype (Eq, Ord, Read, Show, IsString)
newtype CertChainFilePaths = CertChainFilePaths
  { unCertChainFilePaths :: [CertChainFilePath]}
  deriving (Eq, Ord)
_tlsCertChainFilePaths :: Lens' ServantOptions [FilePath]
_tlsCertChainFilePaths
  = _tls
  . lens
    (fmap unCertChainFilePath . unCertChainFilePaths . certChainFilePaths)
    (\x y -> x { certChainFilePaths = CertChainFilePaths . fmap CertChainFilePath $ y })
instance Read CertChainFilePaths where
  readsPrec _ input = [ (CertChainFilePaths . mapMaybe (readMaybe . unpack) . split ',' . fromString $ input, "")]
instance Show CertChainFilePaths where
  show = List.intercalate "," . fmap show . unCertChainFilePaths
instance IsString CertChainFilePaths where
  fromString = fromMaybe (CertChainFilePaths []) . readMaybe
instance HasParser CertChainFilePaths where
  parser =
    strOption
    $ long "sTcc"
    <> metavar "Chain Files"
    <> value (CertChainFilePaths [])
    <> help "set Servant TLS Certificate Chain File Paths"
