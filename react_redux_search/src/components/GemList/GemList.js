// @flow

import React from 'react'
import GemItem from './GemItem'
import type { State as GemItemState
            , Handlers as GemItemHandlers
            } from './GemItem'

export type State = Array<GemItemState>

export type Handlers =
  { toggleSave : string => void }

export type Config =
  { title : string }

type Props =
  { state : State
  , handlers : Handlers
  }

const GemList = (config : Config) => (props : Props) =>  {
    const { title } = config
    const gems = props.state
    const { toggleSave } = props.handlers
    return (
      <div>
        <h3>{title}</h3>
        { gems.map( gem => {
            const state : GemItemState = gem
            const handlers : GemItemHandlers =
              { onClick : _ => toggleSave(gem.name) }

            return <GemItem
              key={ gem.name }
              state={ state }
              handlers={ handlers }
              />
          } )
        }
      </div>
    )
}

export default GemList
