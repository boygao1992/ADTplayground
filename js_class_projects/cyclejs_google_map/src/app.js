/* global R */

import xs from 'xstream'
import { div, label, input, hr } from '@cycle/dom'
import debounce from 'xstream/extra/debounce'

function intent(sources) {
  const domSource = sources.DOM
  const httpSource = sources.HTTP
  const mapSource = sources.MAP
  const timeSource = sources.Time

  const input$ = domSource.select('.field')
    .events('input')
  return {
    term$: input$
      .compose(timeSource.debounce(500))
      .map(ev => ev.target.value)
      .filter(query => query.length > 0),
    tweets$: httpSource.select('tweets')
      .flatten()
      .map(({ body: tweets }) =>
        R.map(({ _source: tweet }) =>
          ({ ...tweet,
            coordinates: {
              lat: tweet.coordinates.lat,
              lng: tweet.coordinates.lon,
            }
          }), tweets)),
    center$: mapSource.centerMarker.position_changed
      .compose(timeSource.debounce(500)),
  }
}

export function App(sources) {

  const actions = intent(sources)
  const term$ = actions.term$
  const tweets$ = actions.tweets$
  const center$ = actions.center$

  const termRequest$ = term$.map(term => ({
    url: `/term/${term}`,
    method: 'GET',
    responseType: 'json',
    category: 'term',
  }))
  const geoRequest$ = xs.combine(term$, center$)
    .map(([term, coordinate]) => ({
      url: `/term/${term}/lat/${coordinate.lat}/lon/${coordinate.lon}`,
      method: 'GET',
      responseType: 'json',
      category: 'termGeo',
    }))
  const request$ = xs.merge(termRequest$, geoRequest$).map(request => ({
    ...request,
    category: 'tweets',
  }))
  const vdom$ = xs.of(
    div([
      label('.label', 'Search:'),
      input('.field', {
        attrs: {
          type: 'text'
        }
      }),
      div('keywords: cat, dog, trump, love, girl, etc.'),
      div([
        div('Usage:'),
        div('Type in a keyword. Keyword can be changed on the fly. The query will be executed when no change being made in 500ms.'),
        div('Click on map for distance filtering. Center marker is draggable and replaceable. The geo query will be executed when Center maker is settled for 500ms.'),
        div('Click on any of the markers. An info window will pop up and show the content of the tweet.')
      ]),
      hr(),
      div('#map', {
        style: {
          width: '100%',
          height: '400px',
        }
      }),
    ])
  )
  const gmap$ = tweets$.startWith({
    center: {
      lat: 40.7,
      lng: -74,
    }
  })

  const sinks = {
    DOM: vdom$,
    HTTP: request$,
    MAP: gmap$,
  }
  return sinks
}
