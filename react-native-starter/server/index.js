const express = require('express')
const cors = require('cors')
const app = express()

app.use(cors())
app.use(express.json())

app.get('/gems', (req, res) => {
  console.log('gems access')
  res.json([ { title: "one" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
             { title: "two" },
           ])
})

app.listen(8080, () => {
  console.log("listening on 8080")
} )
