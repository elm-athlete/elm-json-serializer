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

import String.Extra as String
import Aliases exposing (..)
import Generator.Module
import Generator.Decoder
import Generator.Encoder

type alias Model =
  { typesToGenerate : List (ModuleName, String)
  , rawFiles : Dict ModuleName RawFile
  , filesContent : Dict ModuleName DecodersEncoders
  }

addTypeNameToGenerate : (ModuleName, String) -> Model -> Model
addTypeNameToGenerate typeName ({ typesToGenerate } as model) =
  { model | typesToGenerate = typeName :: typesToGenerate }

addRawFile : ModuleName -> RawFile -> Model -> Model
addRawFile moduleName rawFile ({ rawFiles } as model) =
  { model | rawFiles = Dict.insert moduleName rawFile rawFiles }

type Msg
  = FileContentRead (String, String)
  | GenerateDecodersEncoders
  | SendErrorMessage String

port fileContentRead : ((FileContent, TypeName) -> msg) -> Sub msg
port writeFile : (Decoder, Encoder, FileName) -> Cmd msg
port killMePleaseKillMe : Bool -> Cmd msg
port theresAnErrorDude : String -> Cmd msg

type alias DecodersEncoders =
  { decoders : List String
  , encoders : List String
  }

type alias DecodersEncodersDeps =
  { decoder : String
  , encoder : String
  , decoderDeps : List (ModuleName, String)
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
  (Model [] Dict.empty Dict.empty, Cmd.none)

updateAndThen : Msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateAndThen msg (model, cmd) =
  let (newModel, newCmd) = update msg model in
  (newModel, Cmd.batch [ newCmd, cmd ])

sendErrorMessage : String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
sendErrorMessage error = updateAndThen (SendErrorMessage error)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ rawFiles, typesToGenerate, filesContent } as model) =
  case msg of
    SendErrorMessage error -> (model, theresAnErrorDude error)
    GenerateDecodersEncoders ->
      case List.head typesToGenerate of
        Nothing -> (model, writeGeneratedFiles filesContent)
        Just (moduleName, typeName) ->
          generateDecodersAndEncoders moduleName typeName model
    FileContentRead (value, name) ->
      updateAndThen GenerateDecodersEncoders <|
        parseFileAndStoreContent value name model

writeGeneratedFiles : Dict ModuleName DecodersEncoders -> Cmd Msg
writeGeneratedFiles filesContent =
  filesContent
  |> Dict.toList
  |> List.map writeFileContent
  |> Cmd.batch

writeFileContent : (String, DecodersEncoders) -> Cmd Msg
writeFileContent (moduleName, { decoders, encoders }) =
  writeFile
    ( decoders
      |> String.newlineJoin
      |> Generator.Module.addModuleName moduleName Generator.Module.Decoder
    , encoders
      |> String.newlineJoin
      |> Generator.Module.addModuleName moduleName Generator.Module.Encoder
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
            |> addTypeNameToGenerate (joinedModuleName, name)
            |> addRawFile joinedModuleName rawFile
          , Cmd.none
          )

generateDecodersAndEncoders : ModuleName -> TypeName -> Model -> (Model, Cmd Msg)
generateDecodersAndEncoders moduleName typeName ({ rawFiles, typesToGenerate, filesContent } as model) =
  case Dict.get moduleName rawFiles of
    Nothing -> sendErrorMessage "NoFile, what happened?" (model, Cmd.none)
    Just rawFile ->
      updateAndThen GenerateDecodersEncoders <|
        ( rawFile
          |> extractType typeName
          |> Maybe.andThen generateDecodersEncodersAndDeps
          |> Maybe.map (storeDecodersEncodersAndDepsIn model moduleName)
          |> Maybe.withDefault model
        , Cmd.none
        )

storeDecodersEncodersAndDepsIn : Model -> ModuleName -> DecodersEncodersDeps -> Model
storeDecodersEncodersAndDepsIn ({ typesToGenerate, filesContent } as model) moduleName { decoder, encoder, decoderDeps } =
  let { encoders, decoders } = filesContent
                               |> Dict.get moduleName
                               |> Maybe.withDefault { encoders = [], decoders = [] } in
  { model
    | filesContent = Dict.insert moduleName
      { decoders = List.append decoders [ decoder ]
      , encoders = List.append encoders [ encoder ]
      } filesContent
    , typesToGenerate = List.append decoderDeps (Maybe.withDefault [] (List.tail typesToGenerate))
  }

extractType : String -> RawFile -> Maybe Declaration.Declaration
extractType name rawFile =
  Elm.Processing.process Elm.Processing.init rawFile
  |> .declarations
  |> List.map Tuple.second
  |> List.filter isAliasOrType
  |> findDeclarationByName name

isAliasOrType : Declaration.Declaration -> Bool
isAliasOrType declaration =
  case declaration of
    Declaration.AliasDecl decl -> True
    Declaration.TypeDecl decl -> True
    _ -> False

findDeclarationByName : String -> List Declaration.Declaration -> Maybe Declaration.Declaration
findDeclarationByName name declarations =
  case declarations of
    Declaration.AliasDecl decl :: tl ->
      if decl.name == name then
        Just (Declaration.AliasDecl decl)
      else
        findDeclarationByName name tl
    Declaration.TypeDecl decl :: tl ->
      if decl.name == name then
        Just (Declaration.TypeDecl decl)
      else
        findDeclarationByName name tl
    _ :: tl ->
      findDeclarationByName name tl
    [] ->
      Nothing

generateDecodersEncodersAndDeps : Declaration.Declaration -> Maybe DecodersEncodersDeps
generateDecodersEncodersAndDeps declaration =
  case declaration of
    Declaration.AliasDecl decl ->
      let (decoder, deps) = Generator.Decoder.generateAliasDecoderAndDeps decl
          encoder = Generator.Encoder.generateAliasEncoderAndDeps decl in
      Just { decoder = decoder
           , encoder = encoder
           , decoderDeps = deps
           }
    Declaration.TypeDecl decl ->
      Nothing
    _ ->
      Nothing

subscriptions : Model -> Sub Msg
subscriptions model =
  fileContentRead FileContentRead
