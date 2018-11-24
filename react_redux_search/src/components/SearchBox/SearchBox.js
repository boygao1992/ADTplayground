// @flow

import React from 'react'

export type State = string

export type Handlers =
  { onValueChange : string => void }

type Props =
  { state : State
  , handlers : Handlers
}

const SearchBox = (props: Props) => {
  const value = props.state
  const { onValueChange } = props.handlers
  return (
    <input
      type="text"
      value={ value }
      onChange={ e => onValueChange(e.target.value) }
      />
  )
}

export default SearchBox
