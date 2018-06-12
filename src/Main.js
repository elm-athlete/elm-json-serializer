const { Elm } = require('../dist/app.js')
const app = Elm.Main.worker()

const filePath = process.argv[2]
const type = process.argv[3]
const cwd = process.cwd()
const completeFilePath = cwd + '/' + filePath

const fs = require('fs')
const readedFile = fs.readFile(completeFilePath, function(err, res) {
  app.ports.fromJs.send([ res.toString(), type ])
})
