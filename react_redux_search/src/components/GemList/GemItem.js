// @flow
import React from 'react'

export type State =
  {| name : string
   , authors : Array<string>
   , info : string
   , version : string
   , downloads: number
   , saved : boolean
   |}

export type Handlers
  = { onClick : () => void }

type Props =
  { state : State
  , handlers : Handlers
  }

const GemItem = (props : Props) => {
  const { name
        // , authors
        , info
        , version
        , downloads
        , saved
        } = props.state

  const { onClick } = props.handlers

  return (
    <div className="gem-item">
      <div className="gem-item__left">
        <h3 className="gem-item__left__title">
          {name}
        </h3>
        <span className="gem-item__left__version">
          {version}
        </span>
        <p className="gem-item__left__info">
          {info}
        </p>
      </div>
      <div className="gem-item__right">
        <span className="gem-item__right__downloads-number">
          {downloads}
        </span>
        <span className="gem-item__right__downloads-desc">
          "downloads"
        </span>
        <button className="gem-item__right__save-button"
                onClick={ onClick } >
          <span>{ saved ? "remove" : "save" }</span>
        </button>
      </div>
    </div>
  )
}

export default GemItem
