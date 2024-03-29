module Main where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI) as HD
import Polaris.UI.AppM (Env, runAppM)
import Polaris.UI.Data.Route (Route(..), routeCodec)
import Polaris.UI.Router (Query(..), component)
import Routing.Duplex (parse)
import Routing.Hash (getHash, matchesWith)
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.Location as Location
import Web.HTML.HTMLElement (HTMLElement)

runUI :: HTMLElement -> Aff.Aff Unit
runUI body = do
  halogenIO <- HD.runUI (H.hoist (runAppM { hostname: "" }) component) unit body
  void $ H.liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      Aff.launchAff_ $ halogenIO.query $ H.tell $ Navigate new

reRunUI :: HTMLElement -> Effect Unit
reRunUI body = HA.runHalogenAff $ runUI body

getHostname :: Effect String
getHostname = HTML.window >>= Window.location >>= Location.href -- TODO

main :: Effect Unit
main = do
  hostname <- getHostname
  let
    (env :: Env) = { hostname }
    rootComponent = H.hoist (runAppM env) component

  initialHash <- getHash
  let
    (initialRoute :: Route)
      = fromMaybe Home <<< hush <<< parse routeCodec $ initialHash

  HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <- HD.runUI rootComponent unit body
    void $ H.liftEffect $ matchesWith (parse routeCodec) \old new ->
      when (old /= Just new) do
        Aff.launchAff_ $ halogenIO.query $ H.tell $ Navigate new
