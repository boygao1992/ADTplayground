module Polaris.UI.AppM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT(..), runReaderT, asks)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Polaris.UI.Capability.Navigate (class Navigate, navigate)
import Polaris.UI.Data.Route as Route
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

type Env =
  { hostname :: String
  }

newtype AppM a = AppM (ReaderT Env Aff a)
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec
