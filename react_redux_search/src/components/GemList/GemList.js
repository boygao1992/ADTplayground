// @flow

import React from 'react'
import GemItem from './GemItem'
import type { State as GemItemState
            , Handlers as GemItemHandlers
            } from './GemItem'

export type State = Array<GemItemState>
export type Handlers =
  { toggleSave : string => void }

type Props =
  { state : State
  , handlers : Handlers
  }

const GemList = (props : Props) =>  {
    const gems = props.state
    const { toggleSave } = props.handlers
    return (
      <div>
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
