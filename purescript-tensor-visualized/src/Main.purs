module Main where

import Prelude

import Echarts.Chart (adjacencyMatrixToEdges, adjacencyMatrixToNodes, graphSeriesItem, init, setOption)
import Effect (Effect)
import Halogen (liftEffect)
import Halogen.Aff.Util (awaitBody, runHalogenAff)
import Tensor (i3, o2, tensorProduct)
import Web.HTML (HTMLElement)

runUI :: HTMLElement -> Effect Unit
runUI body = do
  let
    matrix = tensorProduct (*) i3 $ tensorProduct (*) i3 o2
    nodes = adjacencyMatrixToNodes matrix
    edges = adjacencyMatrixToEdges $ map (\i -> i > 0) matrix
    option =
      { tooltip: { show: true }
      , title: []
      , series: [graphSeriesItem nodes edges]
      }
  chart <- init body
  setOption chart option

reRunUI :: HTMLElement -> Effect Unit
reRunUI = runUI

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  liftEffect $ runUI body
