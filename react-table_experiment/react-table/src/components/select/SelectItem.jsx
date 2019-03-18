import React from 'react'

const SelectItem = ({id, title, options, disabled}) => (
  <li className={`form__inputs__${id}`}>
    <h3>{title}</h3>
    <select className={`form__inputs__${id}__select`} disabled={disabled}>
      {options.map(
         ({label, value}) => (
           <option value={value} key={value}> {label} </option>
         )
      )}
    </select>
  </li>
)

export default SelectItem
