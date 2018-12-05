const express = require('express')
const cors = require('cors')
const app = express()

app.use(cors())
app.use(express.json())

app.get('/gems', (req, res) => {
  res.json([ { id: 1, title: "one" },
             { id: 2, title: "two" }
           ])
})

app.listen(8080, () => {
  console.log("listening on 8080")
} )
