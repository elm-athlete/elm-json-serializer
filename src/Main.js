const { Elm } = require('../dist/app.js')
const app = Elm.Main.worker()

const filePath = process.argv[2]
const name = process.argv[3]
const cwd = process.cwd()
const completeFilePath = cwd + '/' + filePath

const fs = require('fs')
const readedFile = fs.readFile(completeFilePath, (err, res) => {
  app.ports.fromJs.send([ res.toString(), name ])
})

const timeout = setTimeout(() => {
  console.error('Timeout, execution took too long to execute. Please, open an issue on GitHub with your use case.')
  process.exit(1)
}, 10000)

app.ports.toJs.subscribe(decoderAndEncoder => {
  const decoder = decoderAndEncoder[0]
  const encoder = decoderAndEncoder[1]
  clearTimeout(timeout)
  console.log(decoder)
  console.log(encoder)
  process.exit(0)
})
