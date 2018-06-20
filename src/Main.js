#!/usr/bin/env node
const { Elm } = require('../dist/app.js')
const app = Elm.Main.worker()

const util = require('util')
const { execSync } = require('child_process')
const commandExists = require('command-exists').sync
const fs = require('fs')
const shell = require('shelljs')
var program = require('commander')

if (!commandExists('elm-format')) {
  console.error('Elm Format is not on your path. Install it before using this software please!')
  process.exit(1)
}

program
  .version('0.1.0')
  .option('-f, --file   <fileInput>', 'File input')
  .option('-t, --type   <typeInput>', 'Type to generate encoders and decoders')
  .option('-o, --output <outputFolder>', 'Output folder')
  .parse(process.argv)

program.on('--help', () => {
  console.log('\n  Examples:\n')
  console.log('    $ elm-serializer --file example/Model.elm --type Model --output example\n')
})

if (process.argv.length == 2) {
  program.help()
}

if (program.file == undefined || program.type == undefined || program.output == undefined) {
  console.error('You miss arguments. You should check what you wrote.')
  process.exit(1)
}

const filePath = program.file
const name = program.type
const generatedFilePath = program.output
const cwd = process.cwd()
const completeFilePath = `${cwd}/${filePath}`
const completeGeneratedFilePath = `${cwd}/${generatedFilePath}`

const pathGenerator = (name) => {
  const filePathGenerateFolder = `${completeGeneratedFilePath}/${name}`
  return {
    root: filePathGenerateFolder,
    decoder: `${filePathGenerateFolder}/Decoder.elm`,
    encoder: `${filePathGenerateFolder}/Encoder.elm`
  }
}

// Check wheter elm.json is present.
const readElmOptions = () => {
  try {
    return JSON.parse(fs.readFileSync(`${cwd}/elm.json`))
  } catch(error) {
    console.error(`It looks like there's no elm.json where you are... Are you sure you're running this program at the root of an elm project?`)
    console.error(`Just in case, you're actually here: ${cwd}.`)
    console.error(`If you're sure to be inside an elm project, it looks like you need to run elm init before anything else!`)
    process.exit(1)
  }
}

const elmOptions = readElmOptions()

const determineSourceDirectories = () => {
  if (elmOptions['type'] === 'package') {
    return [ 'src' ]
  } else {
    return elmOptions['source-directories'].map(source => `${cwd}/${source}`)
  }
}

const sourcePaths = determineSourceDirectories()

// Read the first file.
fs.readFile(completeFilePath, (err, res) => {
  app.ports.fileContentRead.send([ res.toString(), name ])
})

app.ports.theresAnErrorDude.subscribe(value => {
  console.error(value)
  process.exit(1)
})

// Exit the app when asked.
app.ports.killMePleaseKillMe.subscribe(value => {
  process.exit(0)
})

// Write files when elm send them.
app.ports.writeFile.subscribe(decoderAndEncoder => {
  if (decoderAndEncoder === null) {
    console.error(`${name} has not been found in the file you indicated. Please, check your settings.`)
    process.exit(1)
  }
  const decoder = decoderAndEncoder[0]
  const encoder = decoderAndEncoder[1]
  const fileName = decoderAndEncoder[2]
  const paths = pathGenerator(fileName)
  if (fs.existsSync(completeGeneratedFilePath)) {
    if (!fs.existsSync(paths.root)) {
      fs.mkdirSync(paths.root)
    }
    fs.writeFileSync(paths.decoder, decoder)
    fs.writeFileSync(paths.encoder, encoder)
    try {
      execSync(`elm-format ${paths.decoder} --yes`)
      execSync(`elm-format ${paths.encoder} --yes`)
    } catch(error) {
      console.error(`Elm Format failed, here's why: ${error}`)
      process.exit(1)
    }
  }
})

const readFileOrNull = name => {
  try {
    return fs.readFileSync(name)
  } catch(error) {
    return null
  }
}

const checkDependencyExists = tuples => {
  const moduleName = tuples
    .map(([ elem, moduleName]) => moduleName)
    .reduce((acc, elem) => elem, "")
  const results = tuples
    .map(([ elem ]) => !(elem === null))

  if (!(new Set(results)).has(true)) {
    console.error(`Unable to find the dependency ${moduleName}.`)
    process.exit(1)
  }

  return tuples
}

const flattenDependencies = (acc, [ elem, moduleName ]) => {
  if (elem === null) {
    return acc
  } else {
    return acc.concat([[ elem.toString(), moduleName ]])
  }
}

const convertModuleName = ([ pathModuleName, moduleName ]) => {
  return sourcePaths.map(sourcePath => {
    return [ `${sourcePath}/${moduleName}.elm`, moduleName ]
  })
}

const readFiles = tuples => {
  return tuples.map(([ pathModuleName, moduleName ]) => {
    return [ readFileOrNull(pathModuleName), moduleName ]
  })
}

app.ports.readThoseFiles.subscribe(moduleNames => {
  const newModules = moduleNames
    .map(moduleName => [ moduleName.split('.').join('/'), moduleName ])
    .map(convertModuleName)
    .map(readFiles)
    .map(checkDependencyExists)
    .reduce((acc, elem) => acc.concat(elem), [])
    .reduce(flattenDependencies, [])
  app.ports.takeThoseFiles.send(newModules)
})

app.ports.writeStaticFile.subscribe(([ fileName, content ]) => {
  const paths = fileName.split('.')
  const extra = paths.pop()
  const fullPath = `${cwd}/${generatedFilePath}/${paths.join('/')}`
  shell.mkdir('-p', fullPath)
  fs.writeFileSync(
    `${fullPath}/${extra}.elm`,
    content
  )
  execSync(`elm-format ${fullPath}/${extra}.elm --yes`)
})
