// @flow

import request from "request"
import express from "express"
import cors from "cors"
import type { Gem
            , GemName
            , ResponseError
            , FetchGemsResponse
            , SaveGemResponse
            , RemoveGemResponse
            , FetchSavedGemsResponse
            } from "./shared/types"
import crocks from 'crocks'

/* Utils */
const responseError = (res : express$Response, error : string) : void => {
  res
  .status(400)
  .json(( { error } : ResponseError ))
}

/* Constants */
const RUBY_GEMS_URL = "https://rubygems.org"
const PORT = 3001

/* Types */
type Cache =
  { [name : string] : Gem }
type State =
  { saved : Array<Gem>
  , cache : Cache
  }
const initialState : State
 = { saved : []
   , cache : {}
   }

let state : State = initialState

/* Server */
const app = express()

if ( process.env.NODE_ENV === "production" ) {
  app.use( express.static( "dist" ) )
} else {
  app.use( cors() )
}

app.use( express.json() )


type AppResponse<ResponseSchema> = (express$Response, ResponseSchema) => void

const fetchGemsResponse : AppResponse<FetchGemsResponse> = (res, gems) => {
  res.json(gems)
}
app.get( "/api/??/*", ( req : express$Request, res : express$Response ) => {
  console.log("request: " + req.originalUrl)

  const url = `${RUBY_GEMS_URL}${req.originalUrl}`

  request.get( url, ( err, response, body ) => {
    if ( response.statusCode === 400 ) {
      res.json( [] )
    } else {
      const gems : Array<Gem> = JSON.parse(body)

      fetchGemsResponse(res, gems)

      gems.forEach( gem => {
        const { name } = gem
        state.cache[name] = gem
      })
    }
  } )
} )

const saveGemResponse : AppResponse<SaveGemResponse> = (res, response) => {
  res.json(response)
}
app.post( "/gem/add/:name", ( req : express$Request, res : express$Response ) => {
  console.log("request: " + req.originalUrl)

  const name = req.params.name
  if (state.cache[name]) {
    const savedGem = state.cache[name]

    let isSaved = false
    state.saved.forEach( gem => {
      if ( gem.name === name ) {
        isSaved = true
      }
    })
    if (isSaved) {
      saveGemResponse(res, { name, status: "already saved"})
    } else {
      state.saved.push(savedGem)
      saveGemResponse(res, { name, status: "saved" })
    }
  } else {
    responseError(res, "invalid gem name")
  }
})

const removeGemResponse : AppResponse<RemoveGemResponse> = (res, response) => {
  res.json(response)
}
app.post( "/gem/remove/:name", ( req : express$Request, res : express$Response ) => {
  console.log("request: " + req.originalUrl)

  const name = req.params.name
  const isSaved = state.saved.filter(gem => gem.name === name).length > 0
  if (isSaved) {
    state.saved = crocks.filter( (gem : Gem) => gem.name !== name )(state.saved)
    removeGemResponse(res, { name, status: "removed" })
  } else {
    responseError(res, "name is not in the saved list")
  }
})

const fetchSavedGemsResponse : AppResponse<FetchSavedGemsResponse> = (res, response) => {
  res.json(response)
}
app.get( "/gem", ( req : express$Request, res : express$Response ) => {
  console.log("request: " + req.originalUrl)
  fetchSavedGemsResponse(res, state.saved)
})

app.listen( PORT, _ => {
  console.log( `Ruby Gems proxy API is now running at: http://localhost:${PORT}` )
} )
