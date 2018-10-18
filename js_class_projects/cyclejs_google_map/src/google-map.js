/* global google */
import fromEvent from 'xstream/extra/fromEvent'
import adapt from '@cycle/run'
// import adapt from '@cycle/run/lib/adapt'
import xs from 'xstream'
import { div } from '@cycle/dom'

export default function makeGoogleMapDriver(selector) {
  let mapNode
  let map

  let centerMarker
  let centerCircle
  const centerCircleRadius = 10 / 6378.1 * 6378100

  let markers = []
  let infoWindows = []
  // let markers_event = []

  function createMap(data) {
    mapNode = document.querySelector(selector)
    if (!mapNode) {
      throw new Error(`No element '${selector}' found`)
    }

    // initialize map
    const initial_map = {
      zoom: 9,
      center: data.center
    }
    map = new google.maps.Map(mapNode, initial_map)

    // initialize centerMarker and centerCircle
    centerMarker = new google.maps.Marker({
      // map,
      position: data.center,
      label: 'A',
      draggable: true,
    })
    centerMarker.addListener('dragend', function (e) {
      centerCircle.setCenter(e.latLng)
      map.panTo(e.latLng)
    })
    centerCircle = new google.maps.Circle({
      strokeColor: '#FF0000',
      strokeOpacity: 0.8,
      strokeWeight: 2,
      fillColor: '#FF0000',
      fillOpacity: 0.15,
      // map,
      center: data.center,
      radius: centerCircleRadius
    })
    map.addListener('click', function (e) {
      centerMarker.setMap(map)
      centerMarker.setPosition(e.latLng)
      centerCircle.setMap(map)
      centerCircle.setCenter(e.latLng)
      map.panTo(e.latLng)
    })

    // set bounding box restriction around NY state
    const allowedBounds = new google.maps.LatLngBounds(
      new google.maps.LatLng(40.4774, -74.2589),
      new google.maps.LatLng(40.9176, -73.7004),
    )
    let lastValidCenter = map.getCenter()
    google.maps.event.addListener(map, 'center_changed', function () {
      if (allowedBounds.contains(map.getCenter())) {
        // still within valid bounds, so save the last valid position
        lastValidCenter = map.getCenter()
        return
      }
      // not valid anymore => return to last valid position
      map.panTo(lastValidCenter)
    })

  }

  function updateMap(data) {
    markers.forEach(marker => {
      marker.setMap(null)
    })
    markers = []
    infoWindows = []

    data.forEach(tweet => {
      const marker = new google.maps.Marker({
        map,
        position: tweet.coordinates,
      })
      const infowindow = new google.maps.InfoWindow({
        content: `
<div>
  <div>Created at: ${tweet.created_at}</div>
  <div>User: ${tweet.user}</div>
  <div>Text: ${tweet.text}</div>
</div>
`
        // content: JSON.stringify(tweet),
      })
      marker.addListener('click', function () {
        map.setCenter(marker.getPosition())
        infowindow.open(map, marker)
      })
      markers.push(marker)
      infoWindows.push(infowindow)
      // markers_event.push(createEvent(marker))
    })
  }

  function fromMapEvent(el, evName) {
    let mapListener
    return xs.create({
      start(listener) {
        mapListener = el.addListener(evName, (event) => {
          listener.next(event)
        })
      },
      stop() {
        google.maps.event.removeListener(mapListener)
      }
    })
  }

  function createEvent(el) {
    return evName => adapt(fromMapEvent(el, evName))
  }

  function markerPositionChanged() {
    let mapListener
    return xs.create({
      start(listener) {
        mapListener = centerMarker.addListener('position_changed', _ => {
          const position = centerMarker.getPosition()
          listener.next({ lat: position.lat(), lon: position.lng()})
        })
      },
      stop() {
        google.maps.event.removeListener(mapListener)
      }
    })
  }

  function googleMapDriver(sink$) {
    sink$
      .take(1)
      .addListener({
        next: createMap
      })
    sink$
      .drop(1)
      .addListener({
        next: updateMap
      })
    return {
      centerMarker: {
        position_changed: sink$.take(1)
          .map(_ => markerPositionChanged())
          .flatten(),
      }
    }
  }

  return googleMapDriver
}
