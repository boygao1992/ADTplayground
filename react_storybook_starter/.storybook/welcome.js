import React from 'react'

import { storiesOf } from '@storybook/react'

storiesOf('Welcome', module)
  .addWithJSX('Welcome to storybook', _ => (
    <h1>welcome to your new storybook</h1>
  ))
