import React from 'react'

import { withInfo } from '@storybook/addon-info'
import { storiesOf } from '@storybook/react'
import { Button } from './Button'
import { text } from '@storybook/addon-knobs/react'

storiesOf("Button", module)
  .addWithJSX("with background-color and color",
    withInfo({
      inline: true,
      source: true,
      styles: {
        header: {
          h1: {
            color: "red"
          }
        }
      },
      text: `
      Component Source

      ~~~js
      <button style={{ backgroundColor, color }}>
        { children }
      </button>
      ~~~
      `
    })(
      _ => (
        <Button backgroundColor="#b3b3b3" color="white">Hello world</Button>
      )
    )
  )
  .addWithJSX("with configurable backgroundColor",
    _ => (
      <Button backgroundColor={text("backgroundColor", "#b3b3b3")} color="white">Hello world</Button>
    )
  )
