const { Elm } = require('../dist/app.js')
const app = Elm.Main.worker()

const util = require('util')
const { execSync } = require('child_process')
const commandExists = require('command-exists').sync
const fs = require('fs')
const shell = require('shelljs')

if (!commandExists('elm-format')) {
  console.error('Elm Format is not on your path. Install it before using this software please!')
  process.exit(1)
}

const filePath = process.argv[2]
const name = process.argv[3]
const generatedFilePath = process.argv[4]
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
    console.log(error)
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

// const timeout = () => setTimeout(() => {
//   console.error('Timeout, execution took too long to execute. Please, open an issue on GitHub with your use case.')
//   process.exit(1)
// }, 10000)

// let timeoutValue = timeout()

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
  // clearTimeout(timeoutValue)
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
  // timeoutValue = timeout()
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
  const fullPath = `${cwd}/src/${paths.join('/')}`
  shell.mkdir('-p', fullPath)
  fs.writeFileSync(
    `${fullPath}/${extra}.elm`,
    content
  )
  execSync(`elm-format ${fullPath}/${extra}.elm --yes`)
})
