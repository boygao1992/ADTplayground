{-# LANGUAGE TypeOperators #-}
module Shopify.TestApp.Api where

import RIO
import Servant
import Data.UUID as UUID (toText)
import Data.UUID.V4 as UUID (nextRandom)
import Database.Beam.Query

import Tonatona.Servant.Run (PostCreatedRedirect, redirect)
import Tonatona.Shopify.Options (HasShopifyOptions, shopifyOptionsL, _baseUrl, _apiKey)
import Tonatona.Beam.MySQL.Resources (HasBeamMySQLResources)
import Tonatona.Beam.MySQL.Run (runBeamMySQLDebug, runBeamMySQLDebugSafe)

import Shopify.Api.Admin.OAuth (oAuthAuthorizeUrl)
import Shopify.Api.Admin.OAuth.Data.Req.OAuthAuthorize as OAuthAuthorize (Req(..))
import Shopify.Api.Admin.Data.Scopes (Scopes(..), Scope(..))
import Shopify.TestApp.Database
import qualified Shopify.TestApp.Database.Oauth as O


----------
-- RestApi

type RestApi
  = Add
  :<|> Install
  :<|> InstallConfirm

type Add
  = "add"
  :> QueryParam' [Required, Strict] "shop" Text
  :> PostCreatedRedirect Text

add
  :: ( HasShopifyOptions env
     , HasBeamMySQLResources env
     , HasLogFunc env
     )
  => Text
  -> RIO env (Headers '[Header "Location" Text] NoContent)
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
  redirect redirect_url


type Install
  = "install"
  :> QueryParam' [Required, Strict] "code" Text
  :> QueryParam' [Required, Strict] "hmac" Text
  :> QueryParam' [Required, Strict] "timestamp" Text
  :> QueryParam' [Required, Strict] "state" Text
  :> QueryParam' [Required, Strict] "shop" Text
  :> PostCreatedRedirect Text

install
  :: ( HasShopifyOptions env
     , HasBeamMySQLResources env
     , HasLogFunc env
     )
  => Text -> Text -> Text -> Text -> Text
  -> RIO env (Headers '[Header "Location" Text] NoContent)
install code hmac timestamp nonce1 hostname = do
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

  -- TODO
  redirect ""


type InstallConfirm
  = "install-confirm"
  :> QueryParam "hmac" Text
  :> QueryParam "code" Text
  :> QueryParam "timestamp" Text
  :> QueryParam "nonce" Text
  :> QueryParam "shop" Text
  :> PostCreatedRedirect Text

------
-- Api

type Api
  = RestApi
  :<|> Raw
