const { Elm } = require('../dist/app.js')
const app = Elm.Main.worker()

const util = require('util')
const { execSync } = require('child_process')
const commandExists = require('command-exists').sync

if (!commandExists('elm-format')) {
  console.error('Elm Format is not on your path. Install it before using this software please!')
  process.exit(1)
}

const filePath = process.argv[2]
const name = process.argv[3]
const generatedFilePath = process.argv[4]
const cwd = process.cwd()
const completeFilePath = cwd + '/' + filePath
const completeGeneratedFilePath = cwd + '/' + generatedFilePath
const filePathGenerateFolder = completeGeneratedFilePath + '/' + name
const decoderPath = filePathGenerateFolder + '/Decoder.elm'
const encoderPath = filePathGenerateFolder + '/Encoder.elm'

const fs = require('fs')
const readedFile = fs.readFile(completeFilePath, (err, res) => {
  app.ports.fromJs.send([ res.toString(), name ])
})

const timeout = setTimeout(() => {
  console.error('Timeout, execution took too long to execute. Please, open an issue on GitHub with your use case.')
  process.exit(1)
}, 10000)

app.ports.toJs.subscribe(decoderAndEncoder => {
  if (decoderAndEncoder === null) {
    console.error(name, ' has not been found in the file you indicated. Please, check your settings.')
    process.exit(1)
  }
  const decoder = decoderAndEncoder[0]
  const encoder = decoderAndEncoder[1]
  clearTimeout(timeout)
  if (fs.existsSync(completeGeneratedFilePath)) {
    if (!fs.existsSync(filePathGenerateFolder)) {
      fs.mkdirSync(filePathGenerateFolder)
    }
    fs.writeFileSync(decoderPath, decoder)
    fs.writeFileSync(encoderPath, encoder)
    try {
      execSync('elm-format ' + decoderPath + ' --yes')
      execSync('elm-format ' + encoderPath + ' --yes')
    } catch(error) {
      console.error('Elm Format failed, here\'s why: ', error)
      process.exit(1)
    }
  }
  process.exit(0)
})
