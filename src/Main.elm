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
import Generator.Decoder

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
  -- | CompileDependencies (List String)
  | GenerateDecodersEncoders
  | SendErrorMessage String

port fileContentRead : ((FileContent, TypeName) -> msg) -> Sub msg
port writeFile : (Decoder, Encoder, FileName) -> Cmd msg
port killMePleaseKillMe : Bool -> Cmd msg
port theresAnErrorDude : String -> Cmd msg

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
        Nothing -> (model, Cmd.batch (writeFiles filesContent))
        Just (moduleName, typeName) ->
          updateAndThen GenerateDecodersEncoders <|
            generateDecodersAndEncoders moduleName typeName model
    FileContentRead (value, name) ->
      updateAndThen GenerateDecodersEncoders <|
        parseFileAndStoreContent value name model
    -- CompileDependencies dependencies ->
    --   let depsAndTemps = List.map (findDependencyInParsedFiles parsedFiles) (Debug.log "thee" dependencies) in
    --   ( List.foldr addNamesToGenerate model dependencies
    --   , if List.length dependencies == 0 then
    --       killMePleaseKillMe True
    --     else
    --       depsAndTemps
    --       |> List.concatMap Tuple.first
    --       |> List.append
    --         [ depsAndTemps
    --           |> List.concatMap Tuple.second
    --           |> List.map (Result.map (Maybe.map (Tuple.first >> Tuple.second)))
    --           |> List.map Result.toMaybe
    --           |> List.map (Maybe.withDefault Nothing)
    --           |> List.map (Maybe.map List.singleton)
    --           |> List.concatMap (Maybe.withDefault [])
    --           |> List.concat
    --           |> (Task.succeed >> Task.perform CompileDependencies)
    --         ]
    --       |> Cmd.batch
    --   )

writeFiles : Dict ModuleName DecodersEncoders -> List (Cmd Msg)
writeFiles filesContent =
  filesContent
  |> Dict.toList
  |> List.map writeFileContent

writeFileContent : (String, DecodersEncoders) -> Cmd Msg
writeFileContent (moduleName, { decoders, encoders }) =
  writeFile (String.join "\n" decoders, String.join "\n" encoders, moduleName)

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
    , typesToGenerate = List.append (List.map (\a -> ("", a)) decoderDeps) (Maybe.withDefault [] (List.tail typesToGenerate))
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
      let (decoder, deps) = Generator.Decoder.generateAliasDecoderAndDeps decl in
      Just { decoder = decoder
           , encoder = aliasDeclEncoderFun decl.name decl
           , decoderDeps = deps
           }
    Declaration.TypeDecl decl ->
      Nothing
    _ ->
      Nothing

type alias DecodersEncoders =
  { decoders : List String
  , encoders : List String
  }

type alias DecodersEncodersDeps =
  DecodersEncodersDepsInternal

type alias DecodersEncodersDepsInternal =
  { decoder : String
  , encoder : String
  , decoderDeps : List String
  }

type alias ReturnTuple =
  Maybe ((String, List String), String)

addModuleName : String -> Bool -> String -> String
addModuleName name decoder content =
  generateFileContent name content <|
    if decoder then
      GenerationRequirements
        "Decoder"
        "Json.Decode as Decode"
        andMapFunction
    else
      GenerationRequirements
        "Encoder"
        "Json.Encode as Encode"
        ""

type alias GenerationRequirements =
  { moduleNamespace : String
  , imported : String
  , andMap : String
  }

generateFileContent : String -> String -> GenerationRequirements -> String
generateFileContent name content { moduleNamespace, imported, andMap } =
  [ moduleGeneration name moduleNamespace
  , String.spaceJoin [ "import", imported ]
  , andMap
  , content
  ]
  |> String.newlineJoin

moduleGeneration : String -> String -> String
moduleGeneration name moduleNamespace =
  [ "module"
  , String.join "." [ name, moduleNamespace ]
  , "exposing (..)"
  ]
  |> String.spaceJoin

addFileName : FileName -> (Decoder, Encoder) -> Maybe (Decoder, Encoder, FileName)
addFileName fileName (decoder, encoder) =
  Just (decoder, encoder, fileName)

subscriptions : Model -> Sub Msg
subscriptions model =
  fileContentRead FileContentRead

aliasDeclEncoder : Alias.TypeAlias -> String
aliasDeclEncoder { name, typeAnnotation } =
  [ "Encode.object"
  , typeAnnotation
    |> Tuple.second
    |> typeAnnotationEncoder
    |> String.surroundByBrackets
  ]
  |> String.spaceJoin
  |> String.surroundByParen

aliasDeclEncoderFun : String -> Alias.TypeAlias -> String
aliasDeclEncoderFun name declaration =
  let functionName = String.camelize name ++ "Encoder" in
  [ [ functionName, ":", name, "-> Encode.Value" ]
  , [ functionName, "record =" ]
  , [ String.indent (aliasDeclEncoder declaration) ]
  ]
  |> List.map String.spaceJoin
  |> String.newlineJoin

typeAnnotationEncoder : Annotation.TypeAnnotation -> String
typeAnnotationEncoder typeAnnotation =
  case typeAnnotation of
    Annotation.Record definition -> recordEncoder definition
    Annotation.GenericType type_ -> genericTypeEncoder type_
    Annotation.Typed moduleName value annotations -> typedEncoder moduleName value annotations
    -- Annotation.Unit ->
    -- Annotation.Tupled annotations ->
    -- Annotation.GenericRecord name definition ->
    -- Annotation.FunctionTypeAnnotation annotation annotation ->
    _ -> ""

genericTypeEncoder : String -> String
genericTypeEncoder type_ =
  case type_ of
    "String" -> "Encode.string"
    "Int" -> "Encode.int"
    "Float" -> "Encode.float"
    "Bool" -> "Encode.bool"
    value -> "encode" ++ value

typedEncoder : List String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
typedEncoder moduleName type_ annotations =
  genericTypeEncoder type_

recordEncoder : Annotation.RecordDefinition -> String
recordEncoder definition =
  definition
  |> List.map recordFieldEncoder
  |> String.join "\n, "

recordFieldEncoder : (String, (Range.Range, Annotation.TypeAnnotation)) -> String
recordFieldEncoder (name, (_, content)) =
  [ String.surroundByQuotes name
  , String.surroundByParen (typeAnnotationEncoder content ++ " record." ++ name)
  ]
  |> String.join ", "
  |> String.surroundByParen

extractRecord : Annotation.TypeAnnotation -> Maybe Annotation.RecordDefinition
extractRecord annotation =
  case annotation of
    Annotation.Record definition -> Just definition
    _ -> Nothing

andMapFunction : String
andMapFunction =
  [ "andMap : Decoder a -> Decoder (a -> b) -> Decoder b"
  , "andMap = Decode.map2 (|>)"
  ]
  |> String.newlineJoin
