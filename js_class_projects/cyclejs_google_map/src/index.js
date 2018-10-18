import { run } from '@cycle/run'
import { makeDOMDriver } from '@cycle/dom'
import { timeDriver } from '@cycle/time'
import { makeHTTPDriver } from '@cycle/http'
import { App } from './app'
import makeGoogleMapDriver from './google-map'

const main = App

const drivers = {
  DOM: makeDOMDriver('#root'),
  MAP: makeGoogleMapDriver('#map'),
  Time: timeDriver,
  HTTP: makeHTTPDriver(),
}

run(main, drivers)
