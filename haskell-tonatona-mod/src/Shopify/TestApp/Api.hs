{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
module Shopify.TestApp.Api where

import RIO
import qualified RIO.Text as Text
import Servant
import Data.UUID as UUID (toText)
import Data.UUID.V4 as UUID (nextRandom)
import Database.Beam.Query
import Lucid
import Servant.HTML.Lucid (HTML)
import Data.Format
import Data.Default (def)

import Tonatona.Servant.Run (TemporaryRedirect, redirect)
import Tonatona.Shopify.Options (shopifyOptionsL, _baseUrl, _apiKey, _apiSecret, _appName)
import Tonatona.Beam.MySQL.Run (runBeamMySQLDebug, runBeamMySQLDebugSafe)

import Shopify.Api.Admin.OAuth (oAuthAuthorizeUrl, getAccessToken)
import Shopify.Api.Admin.OAuth.Data.AccessToken (AccessToken(..))
import qualified Shopify.Api.Admin.OAuth.Data.Req.OAuthAuthorize as OAuthAuthorize (Req(..))
import qualified Shopify.Api.Admin.OAuth.Data.Req.OAuthAccessToken as OAuthAccessToken (Req(..), Res(..), _access_token)
import Shopify.Api.Admin.Data.Scopes (Scopes(..), Scope(..))

import Shopify.TestApp.Database
import Shopify.TestApp.Database.Oauth

import Shopify.Servant.Client.Util (runBaseHttpClient, runApiHttpClient)
import Shopify.TestApp.Types (Resources)

import Shopify.Data.Products.Product (Products)
import qualified Shopify.Api.Products as GetProducts (getProducts)

-------------
-- App Config

app_scopes :: Scopes
app_scopes = Scopes
  [ ReadThemes
  , WriteThemes
  , ReadProducts
  , WriteProducts
  , ReadProductListings
  , ReadCustomers
  , WriteCustomers
  , ReadOrders
  , WriteOrders
  , ReadDraftOrders
  , WriteDraftOrders
  , ReadScriptTags
  , WriteScriptTags
  , ReadCheckouts
  , WriteCheckouts
  , ReadPriceRules
  , WritePriceRules
  , UnauthenticatedWriteCheckouts
  , UnauthenticatedReadCheckouts
  ]

----------
-- RestApi

type RestApi
  = Home
  :<|> Login
  :<|> Install
  :<|> GetProducts

restApiServer :: ServerT RestApi (RIO Resources)
restApiServer
  = home
  :<|> login
  :<|> install
  :<|> getProducts

type Login
  = "login"
  :> QueryParam' [Required, Strict] "shop" Text
  :> Get '[JSON] (Maybe Text)

login :: Text -> RIO Resources (Maybe Text)
login hostname = do
  opts <- view shopifyOptionsL
  nonce <- UUID.toText <$> liftIO UUID.nextRandom

  mToken
    <- fmap join
    . runBeamMySQLDebug
    $ runSelectReturningOne
    $ select do
        table_oauth <- all_ (testappDb^.oauth)
        guard_ $ table_oauth^.oauthShopname ==. val_ hostname
        pure $ table_oauth^.oauthAccessToken

  if isJust mToken
    then do
      -- base_url <- view (shopifyOptionsL._baseUrl)
      -- redirect $ base_url <> "home"
      pure Nothing

    else do
      runBeamMySQLDebugSafe
        $ runInsert
        $ insert (testappDb^.oauth)
        $ insertValues [ Oauth hostname Nothing (Just nonce) ]

      let
        install_redirect_url :: Text
          = opts^._baseUrl <> "install"

        redirect_url :: Text
          = oAuthAuthorizeUrl
            ([fmt|https://$hostname/|])
            ( OAuthAuthorize.Req
              (Just $ opts^._apiKey)
              (Just $ app_scopes)
              (Just $ install_redirect_url)
              (Just $ nonce)
              []
            )
      logDebug $ display redirect_url
      pure $ Just redirect_url

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
      guard_ $ table_oauth^.oauthShopname ==. val_ hostname
      pure $ table_oauth^.oauthNonce

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
        [ table^.oauthAccessToken <-. just_ (val_ res)
        , table^.oauthNonce <-. nothing_
        ]
      )
      (\table -> table^.oauthShopname ==. val_ hostname)

  appname <- view (shopifyOptionsL._appName)
  redirect $ [fmt|https://$hostname/admin/apps/$appname|]

type Home
  = "home"
  :> QueryParam' [Required, Strict] "shop" Text
  :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))

home :: Text -> RIO Resources (Headers '[Header "Set-Cookie" Text] (Html ()))
home hostname = pure $ addHeader [fmt|hostname=$hostname; SameSite=Strict; Secure; HttpOnly|] $ doctypehtml_ do
  head_ do
    meta_ [charset_ "utf-8"]
    meta_
      [ name_ "viewport"
      , content_ "width=device-width, initial-scale=1"
      ]
    link_
      [ rel_ "icon"
      , href_ "assets/favicon.ico"
      ]
    link_
      [ rel_ "stylesheet"
      , href_ "assets/polaris.css"
      , type_ "text/css"
      ]
    title_ "TestApp"
  body_ do
    script_ [src_ "assets/app.js"] ("" :: Text)
    script_ [] [fmt|PS["Main"]["main"]("$hostname")()|]

type GetProducts
  = "products"
  :> QueryParam' [Required, Strict] "shop" Text
  :> Get '[JSON] Products

getProducts :: Text -> RIO Resources Products
getProducts hostname = do
  mToken :: Maybe AccessToken
    <- fmap join
    . runBeamMySQLDebug
    $ runSelectReturningOne
    $ select do
      table_oauth <- all_ (testappDb^.oauth)
      guard_ $ table_oauth^.oauthShopname ==. val_ hostname
      pure $ table_oauth^.oauthAccessToken

  case mToken of
    Nothing -> throwM err401
    Just token ->
      runApiHttpClient (Text.unpack hostname) token
      $ GetProducts.getProducts def

------
-- Api

type Api
  = RestApi
  :<|> "assets" :> Raw

server :: ServerT Api (RIO Resources)
server = restApiServer
  :<|> serveDirectoryFileServer "assets"
