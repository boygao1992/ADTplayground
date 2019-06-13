{-# LANGUAGE TypeOperators #-}
module Shopify.TestApp.Api where

import RIO
import qualified RIO.Text as Text
import Servant
import Data.UUID as UUID (toText)
import Data.UUID.V4 as UUID (nextRandom)
import Database.Beam.Query

import Tonatona.Servant.Run (TemporaryRedirect, redirect)
import Tonatona.Shopify.Options (shopifyOptionsL, _baseUrl, _apiKey, _apiSecret)
import Tonatona.Beam.MySQL.Run (runBeamMySQLDebug, runBeamMySQLDebugSafe)

import Shopify.Api.Admin.OAuth (oAuthAuthorizeUrl, getAccessToken)
import Shopify.Api.Admin.OAuth.Data.AccessToken (AccessToken(..))
import qualified Shopify.Api.Admin.OAuth.Data.Req.OAuthAuthorize as OAuthAuthorize (Req(..))
import qualified Shopify.Api.Admin.OAuth.Data.Req.OAuthAccessToken as OAuthAccessToken (Req(..), Res(..), _access_token)
import Shopify.Api.Admin.Data.Scopes (Scopes(..), Scope(..))

import Shopify.TestApp.Database
import qualified Shopify.TestApp.Database.Oauth as O

import Shopify.Servant.Client.Util (runBaseHttpClient)
import Shopify.TestApp.Types (Resources)

----------
-- RestApi

type RestApi
  = Add
  :<|> Install
  :<|> Home

restApiServer :: ServerT RestApi (RIO Resources)
restApiServer
  = add
  :<|> install
  :<|> home

type Add
  = "add"
  :> QueryParam' [Required, Strict] "shop" Text
  :> TemporaryRedirect 'GET Text

add :: Text -> RIO Resources (Headers '[Header "Location" Text] NoContent)
add hostname = do
  opts <- view shopifyOptionsL
  nonce <- UUID.toText <$> liftIO UUID.nextRandom

  runBeamMySQLDebugSafe
    $ runInsert
    $ insert (testappDb^.oauth)
    $ insertValues [ O.Oauth hostname Nothing (Just nonce) ]

  let
    install_redirect_url :: Text
      = opts^._baseUrl <> "install"

    redirect_url :: Text
      = oAuthAuthorizeUrl
        ("https://" <> hostname <> "/")
        ( OAuthAuthorize.Req
          (Just $ opts^._apiKey)
          (Just $ Scopes [ReadProducts])
          (Just $ install_redirect_url)
          (Just $ nonce)
          []
        )
  logDebug $ display redirect_url
  redirect redirect_url

type Install
  = "install"
  :> QueryParam' [Required, Strict] "code" Text
  :> QueryParam' [Required, Strict] "hmac" Text
  :> QueryParam' [Required, Strict] "timestamp" Text
  :> QueryParam' [Required, Strict] "state" Text
  :> QueryParam' [Required, Strict] "shop" Text
  :> TemporaryRedirect 'GET Text

install
  :: Text -> Text -> Text -> Text -> Text
  -> RIO Resources (Headers '[Header "Location" Text] NoContent)
install code _ _ nonce1 hostname = do
  mNonce2 :: Maybe Text
    <- fmap join
    . runBeamMySQLDebug
    $ runSelectReturningOne
    $ select
    do
      table_oauth <- all_ (testappDb^.oauth)
      guard_ $ table_oauth^.O.shopname ==. val_ hostname
      pure $ table_oauth^.O.nonce

  if
    | ((==) <$> pure nonce1 <*> mNonce2) == (Just True) -> pure ()
    | otherwise -> throwM err401


  apiKey <- view (shopifyOptionsL._apiKey)
  apiSecret <- view (shopifyOptionsL._apiSecret)

  res :: AccessToken
    <- fmap OAuthAccessToken._access_token
    . runBaseHttpClient (Text.unpack hostname)
    $ getAccessToken
    $ OAuthAccessToken.Req apiKey apiSecret code

  runBeamMySQLDebugSafe
    $ runUpdate
    $ update (testappDb^.oauth)
      (\table -> mconcat
        [ table^.O.access_token <-. just_ (val_ res)
        , table^.O.nonce <-. nothing_
        ]
      )
      (\table -> table^.O.shopname ==. val_ hostname)

  base_url <- view (shopifyOptionsL._baseUrl)
  redirect $ base_url <> "home"

type Home
  = "home"
  :> Get '[JSON] Text

home :: RIO Resources Text
home = pure "welcome"

------
-- Api

type Api
  = RestApi
  :<|> Raw

server :: ServerT Api (RIO Resources)
server = restApiServer
  :<|> serveDirectoryFileServer "public"
