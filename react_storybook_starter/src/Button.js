import React from 'react'

export const Button = ({ backgroundColor, color, children }) => (
  <button style={{ backgroundColor, color }}>
    { children }
  </button>
)
