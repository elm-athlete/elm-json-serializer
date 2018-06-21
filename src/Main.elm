port module Main exposing (..)

import Dict exposing (Dict)
import Task exposing (Task)
import Json.Decode as Decode
import Elm.Parser
import Elm.RawFile exposing (RawFile)
import Elm.Processing
import Elm.Syntax.Range as Range
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation
import Elm.Syntax.Type as Type
import Elm.Syntax.Module as Module
import Elm.Syntax.Base as Base
import Elm.Syntax.Exposing as Exposing

import String.Extra as String
import Aliases exposing (..)
import Generator
import Generator.Module
import Generator.Static
import Dependency exposing (Dependency(..), DecodersEncodersDeps)
import Declaration

type alias Model =
  { typesToGenerate : List (Dependency, String)
  , rawFiles : Dict ModuleName RawFile
  , filesContent : Dict ModuleName DecodersEncoders
  , generatedTypes : List (Dependency, String)
  }

addTypeNameToGenerate : (Dependency, String) -> Model -> Model
addTypeNameToGenerate typeName ({ typesToGenerate } as model) =
  { model | typesToGenerate = typeName :: typesToGenerate }

addTypesNameToGenerate : List (Dependency, String) -> Model -> Model
addTypesNameToGenerate dependencies model =
  List.foldr addTypeNameToGenerate model dependencies

addRawFile : ModuleName -> RawFile -> Model -> Model
addRawFile moduleName rawFile ({ rawFiles } as model) =
  { model | rawFiles = Dict.insert moduleName rawFile rawFiles }

type Msg
  = FileContentRead (FileContent, TypeName)
  | MultipleFilesContentRead (List (FileContent, ModuleName))
  | GenerateDecodersEncoders
  | SendErrorMessage String

port fileContentRead : ((FileContent, TypeName) -> msg) -> Sub msg
port takeThoseFiles : (List (FileContent, ModuleName) -> msg) -> Sub msg

port writeFile : (Decoder, Encoder, FileName) -> Cmd msg
port writeStaticFile : (FileName, String) -> Cmd msg
port killMePleaseKillMe : Bool -> Cmd msg
port theresAnErrorDude : String -> Cmd msg
port readThoseFiles : List String -> Cmd msg

type alias DecodersEncoders =
  { decoders : List String
  , encoders : List String
  , dependencies : List (Dependency, String)
  }

main : Program () Model Msg
main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init flags =
  (Model [] Dict.empty Dict.empty [], Cmd.none)

updateAndThen : Msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateAndThen msg (model, cmd) =
  let (newModel, newCmd) = update msg model in
  (newModel, Cmd.batch [ newCmd, cmd ])

sendErrorMessage : String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
sendErrorMessage error = updateAndThen (SendErrorMessage error)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let { rawFiles, typesToGenerate, filesContent, generatedTypes } = model in
  case msg of
    SendErrorMessage error -> (model, theresAnErrorDude error)
    GenerateDecodersEncoders ->
      case List.head typesToGenerate of
        Nothing -> (model, writeGeneratedFiles model filesContent)
        Just (dependency, typeName) ->
          if List.member (dependency, typeName) generatedTypes then
            updateAndThen GenerateDecodersEncoders
              (removeFirstTypeToGenerate model, Cmd.none)
          else
            generateDecodersAndEncoders dependency typeName model
    FileContentRead (value, name) ->
      updateAndThen GenerateDecodersEncoders <|
        parseFileAndStoreContent value name model
    MultipleFilesContentRead dependencies ->
        dependencies
        |> List.foldr addReadFilesToRawFiles (model, Cmd.none)
        |> updateAndThen GenerateDecodersEncoders

addReadFilesToRawFiles
   : (FileContent, ModuleName)
  -> (Model, Cmd Msg)
  -> (Model, Cmd Msg)
addReadFilesToRawFiles (content, moduleName) (model_, cmds) =
  let parsedFile = Elm.Parser.parse content in
  case parsedFile of
    Err errors -> sendErrorMessage "Weird" (model_, cmds)
    Ok rawFile -> (addRawFile moduleName rawFile model_, cmds)

writeGeneratedFiles : Model -> Dict ModuleName DecodersEncoders -> Cmd Msg
writeGeneratedFiles model filesContent =
  filesContent
  |> Dict.toList
  |> List.map (writeFileContent model)
  |> List.append
    [ writeStaticFile
      ( Generator.Static.moduleName
      , Generator.Static.jsonDecodeExtra
      )
    ]
  |> Cmd.batch

writeFileContent : Model -> (String, DecodersEncoders) -> Cmd Msg
writeFileContent { rawFiles } (moduleName, { decoders, encoders, dependencies }) =
  writeFile
    ( decoders
      |> String.newlineJoin
      |> Generator.Module.addModuleName rawFiles dependencies moduleName Generator.Module.Decoder
    , encoders
      |> String.newlineJoin
      |> Generator.Module.addModuleName rawFiles dependencies moduleName Generator.Module.Encoder
    , moduleName
    )

parseFileAndStoreContent : String -> String -> Model -> (Model, Cmd Msg)
parseFileAndStoreContent value name model =
  let parsedFile = Elm.Parser.parse value in
  case parsedFile of
    Err errors -> (model, Cmd.none)
    Ok rawFile ->
      let moduleName = Elm.RawFile.moduleName rawFile in
      case moduleName of
        Nothing -> (model, killMePleaseKillMe True)
        Just moduleName_ ->
          let joinedModuleName = String.join "." moduleName_ in
          ( model
            |> addTypeNameToGenerate (InModule joinedModuleName, name)
            |> addRawFile joinedModuleName rawFile
          , Cmd.none
          )

generateDecodersAndEncoders : Dependency -> TypeName -> Model -> (Model, Cmd Msg)
generateDecodersAndEncoders dependency typeName model =
  let { rawFiles, typesToGenerate, filesContent } = model in
  case dependency of
    InModule moduleName ->
      case Dict.get moduleName rawFiles of
        Nothing -> (model, readThoseFiles [ moduleName ])
        Just rawFile ->
          let
            modelWithTypeDecodersAndEncoders =
              model
              |> addGeneratedTypes (dependency, typeName)
              |> storeDecodersEncodersAndDepsIn moduleName
          in
            updateAndThen GenerateDecodersEncoders <|
              ( rawFile
                |> Declaration.getDeclarationByName typeName
                |> Maybe.andThen Generator.generateDecodersEncodersAndDeps
                |> Maybe.map (Dependency.fetchDependencies moduleName rawFile)
                |> Maybe.map modelWithTypeDecodersAndEncoders
                |> Maybe.withDefault (removeFirstTypeToGenerate model)
              , Cmd.none
              )
    InOneOf moduleNames ->
      let dependencies = List.map (fetchRawFile rawFiles) moduleNames in
      if List.member Nothing (List.map Tuple.second dependencies) then
        ( model
        , dependencies
          |> List.concatMap removeReadFiles
          |> readThoseFiles
        )
      else
        updateAndThen GenerateDecodersEncoders <|
          ( model
            |> removeFirstTypeToGenerate
            |> addGeneratedTypes (dependency, typeName)
            |> addTypesNameToGenerate
              (List.map (changeRawFilesAsDependency typeName) dependencies)
          , Cmd.none
          )

fetchRawFile : Dict ModuleName RawFile -> ModuleName -> (ModuleName, Maybe RawFile)
fetchRawFile rawFiles name =
  (name, Dict.get name rawFiles)

changeRawFilesAsDependency : String -> (String, Maybe RawFile) -> (Dependency, String)
changeRawFilesAsDependency typeName =
  Tuple.mapSecond (always typeName) >> Tuple.mapFirst InModule

addGeneratedTypes : (Dependency, String) -> Model -> Model
addGeneratedTypes value ({ generatedTypes } as model) =
  { model | generatedTypes = value :: generatedTypes }

removeFirstTypeToGenerate : Model -> Model
removeFirstTypeToGenerate ({ typesToGenerate } as model) =
  { model | typesToGenerate = Maybe.withDefault [] (List.tail typesToGenerate) }

removeReadFiles : (ModuleName, Maybe RawFile) -> List ModuleName
removeReadFiles (moduleName, rawFile) =
  case rawFile of
    Nothing -> [ moduleName ]
    Just _ -> []

storeDecodersEncodersAndDepsIn : ModuleName -> Model -> DecodersEncodersDeps -> Model
storeDecodersEncodersAndDepsIn moduleName model { decoder, encoder, dependencies } =
  let { typesToGenerate, filesContent, generatedTypes } = model
      fileContent = filesContent
                    |> Dict.get moduleName
                    |> Maybe.withDefault
                      { encoders = []
                      , decoders = []
                      , dependencies = []
                      } in
  { model
    | filesContent = Dict.insert moduleName
      { decoders = List.append fileContent.decoders [ decoder ]
      , encoders = List.append fileContent.encoders [ encoder ]
      , dependencies = List.append fileContent.dependencies dependencies
      } filesContent
    , typesToGenerate =
      typesToGenerate
      |> List.tail
      |> Maybe.withDefault []
      |> List.append
        (List.filter
          (\elem -> not (List.member elem generatedTypes))
          dependencies
        )
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ fileContentRead FileContentRead
    , takeThoseFiles MultipleFilesContentRead
    ]
