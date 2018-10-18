const term = require('terminal-kit').terminal

// wrap terminal-kit.gridMenu(model, callback) in a Promise
const gridMenuAsync = model => {
  return new Promise((res, rej) => {
    term.gridMenu(
      model,
      (err, input) => {
        if (err) { rej(err) }
        res(input.selectedText)
      }
    )
  })
}

// wrap terminal.singleColumnMenu(model, callback) in a Promise
const singleColumnMenuAsync = model => {
  return new Promise((res, rej) => {
    term.singleColumnMenu(
      model,
      (err, input) => {
        if (err) { rej(err) }
        res(input.selectedText)
      }
    )
  })
}

// wrap terminal.inputField(callback) in a Promise
const inputFieldAsync = () => {
  return new Promise((res, rej) => {
    term.inputField(
      (err, input) => {
        if (err) { rej(err) }
        res(input)
      }
    )
  })
}

const TermAsync = { term, gridMenuAsync, singleColumnMenuAsync, inputFieldAsync }

module.exports = TermAsync
