module Echarts.Chart where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Tensor (Matrix(..))
import Web.HTML.HTMLElement (HTMLElement)

foreign import data Chart :: Type

-- newtype Option = Option Foreign

foreign import _init :: EffectFn1 HTMLElement Chart
init :: HTMLElement -> Effect Chart
init dom = runEffectFn1 _init dom

foreign import _setOption :: EffectFn2 Chart Option Unit
setOption :: Chart -> Option -> Effect Unit
setOption chart opt = runEffectFn2 _setOption chart opt

foreign import _debugChart :: EffectFn1 Chart String
debugChart :: Chart -> Effect String
debugChart chart = runEffectFn1 _debugChart chart

type Id = String

type Node =
  { name :: Id
  , value :: Number
  }

type Edge =
  { source :: Id
  , target :: Id
  , weight :: Number
  }

adjacencyMatrixToNodes :: forall a. Matrix a -> Array Node
adjacencyMatrixToNodes (Matrix xss) =
  Array.mapWithIndex (\idx _ -> { name: show idx, value: 1.0 }) xss

adjacencyMatrixToEdges :: Matrix Boolean -> Array Edge
adjacencyMatrixToEdges (Matrix xss) =
  foldlWithIndex
    ( \ix acc xs -> acc <>
        foldlWithIndex
          (\iy row x ->
              if x
              then row <> [{ source: show ix, target: show iy, weight: 1.0 }]
              else row
          )
          []
          xs
    )
    []
    xss

type Option =
  { tooltip :: Tooltip
  , title :: Title
  , series :: Series
  }

type Tooltip =
  { show :: Boolean
  }

type Title = Array TitleItem
type TitleItem =
  { text :: String
  , left :: String
  , top :: String
  , textAlign :: String
  }

type Series = Array SeriesItem

type SeriesItem =
  { "type" :: String
  , layout :: String
  , itemStyle :: ItemStyle
  , nodes :: Array Node
  , edges :: Array Edge
  , left :: String
  , top :: String
  , bottom :: String
  , width :: String
  , force :: Force
  , draggable :: Boolean
  , edgeSymbol :: Array String
  }

type ItemStyle =
  { normal :: ItemStyleNormal
  }

type ItemStyleNormal =
  { borderColor :: String
  , borderWidth :: Number
  }

type LineStyle =
  { normal :: LineStyleNormal
  }

type LineStyleNormal =
  { color :: String
  }

type Force =
  { repulsion :: Number
  }

title :: TitleItem
title =
  { text: "Title"
  , left: "25%"
  , top: "10%"
  , textAlign: "center"
  }

graphSeriesItem :: Array Node -> Array Edge -> SeriesItem
graphSeriesItem nodes edges =
  { "type": "graph"
  , layout: "force"
  , itemStyle:
      { normal:
          { borderColor: "#fff"
          , borderWidth: 2.0
          }
      }
  , nodes
  , edges
  , left: "0"
  , top: "0"
  , width: "100%"
  , bottom: "0"
  , force: {
      repulsion: 50.0
    }
  , draggable: true
  , edgeSymbol: ["none", "arrow"]
  }
