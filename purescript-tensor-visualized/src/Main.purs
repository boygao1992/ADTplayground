module Main where

import Prelude

import Echarts.Chart (adjacencyMatrixToGraph, graphSeriesItem, init, setOption)
import Effect (Effect)
import Halogen (liftEffect)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Tensor (exerciseX, exerciseY, tensorProductBool)
import Web.HTML (HTMLElement)

runUI :: HTMLElement -> Effect Unit
runUI body = do
  let
    xProductY = tensorProductBool exerciseX exerciseY
    option =
      { tooltip: { show: true }
      , title:
          [ { text: "X", left: "12.5%", top: "5%", textAlign: "center" }
          , { text: "Y", left: "12.5%", top: "55%", textAlign: "center" }
          , { text: "X ⨂ Y", left: "62.5%", top: "0%", textAlign: "center"}
          ]
      , series:
          [ graphSeriesItem "X"
            { left: "0", top: "0", width: "25%", height: "50%" }
            $ adjacencyMatrixToGraph exerciseX
          , graphSeriesItem "Y"
            { left: "0", top: "50%", width: "25%", height: "50%" }
            $ adjacencyMatrixToGraph exerciseY
          , graphSeriesItem "X ⨂ Y"
            { left: "25%", top: "0", width: "75%", height:"100%" }
            $ adjacencyMatrixToGraph xProductY
          ]
      }
  chart <- init body
  setOption chart option

reRunUI :: HTMLElement -> Effect Unit
reRunUI = runUI

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  liftEffect $ runUI body
