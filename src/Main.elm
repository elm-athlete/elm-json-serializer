port module Main exposing (..)

import Json.Decode as Decode
import Elm.Parser
import Elm.RawFile exposing (RawFile)
import Elm.Processing
import Elm.Syntax.Range as Range
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation

type alias Model =
  Int

type Msg
  = FromJs (String, String)

port fromJs : ((String, String) -> msg) -> Sub msg
port toJs : Maybe (String, String) -> Cmd msg

type alias ReturnType =
  Maybe { decoder : String
        , encoder : String
        }

returnToTuple : String -> { decoder : String, encoder : String } -> Maybe (String, String)
returnToTuple name { decoder, encoder } =
  Just (addModuleName name True decoder, addModuleName name False encoder)

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
  , spaceJoin [ "import", imported ]
  , andMap
  , content
  ]
  |> newlineJoin

moduleGeneration : String -> String -> String
moduleGeneration name moduleNamespace =
  [ "module"
  , String.join "." [ name, moduleNamespace ]
  , "exposing (..)"
  ]
  |> spaceJoin

main : Program () Model Msg
main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init flags =
  (0, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FromJs (value, name) ->
      ( model
      , value
        |> Elm.Parser.parse
        |> Result.map (extractType name)
        |> Result.map (Maybe.andThen (returnToTuple name))
        |> Result.map toJs
        |> Result.withDefault Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  fromJs FromJs

extractType : String -> RawFile -> ReturnType
extractType name rawFile =
  Elm.Processing.process Elm.Processing.init rawFile
  |> .declarations
  |> List.map Tuple.second
  |> extractFromDeclaration name

extractFromDeclaration : String -> List Declaration.Declaration -> ReturnType
extractFromDeclaration name declarations =
  let declaration = declarations
                    |> List.concatMap keepAliasDecl
                    |> findByName name in
  declaration
  |> Maybe.andThen (createReturnType name)

createReturnType : String -> Alias.TypeAlias -> ReturnType
createReturnType name declaration =
  Just { decoder = aliasDeclDecoderFun name declaration
       , encoder = aliasDeclEncoderFun name declaration
       }

aliasDeclEncoder : Alias.TypeAlias -> String
aliasDeclEncoder { name, typeAnnotation } =
  [ "Encode.object"
  , typeAnnotation
    |> Tuple.second
    |> typeAnnotationEncoder
    |> surroundByBrackets
  ]
  |> spaceJoin
  |> surroundByParen

aliasDeclEncoderFun : String -> Alias.TypeAlias -> String
aliasDeclEncoderFun name declaration =
  let functionName = camelize name ++ "Encoder" in
  [ [ functionName, ":", name, "-> Encode.Value" ]
  , [ functionName, "record =" ]
  , [ indent (aliasDeclEncoder declaration) ]
  ]
  |> List.map spaceJoin
  |> newlineJoin

aliasDeclDecoder : Alias.TypeAlias -> String
aliasDeclDecoder { name, typeAnnotation } =
  [ spaceJoin [ "Decode.succeed", name, newline "|> andMap" ]
  , typeAnnotation
    |> Tuple.second
    |> typeAnnotationDecoder
  ]
  |> spaceJoin
  |> surroundByParen

aliasDeclDecoderFun : String -> Alias.TypeAlias -> String
aliasDeclDecoderFun name declaration =
  let functionName = camelize name ++ "Decoder" in
  [ [ functionName, ": Decoder", name ]
  , [ functionName, "=" ]
  , [ indent (aliasDeclDecoder declaration) ]
  ]
  |> List.map spaceJoin
  |> newlineJoin

typeAnnotationDecoder : Annotation.TypeAnnotation -> String
typeAnnotationDecoder typeAnnotation =
  case typeAnnotation of
    Annotation.Record definition -> recordDecoder definition
    Annotation.GenericType type_ -> genericTypeDecoder type_
    Annotation.Typed moduleName value annotations -> typedDecoder moduleName value annotations
    -- Annotation.Unit ->
    -- Annotation.Tupled annotations ->
    -- Annotation.GenericRecord name definition ->
    -- Annotation.FunctionTypeAnnotation annotation annotation ->
    _ -> ""

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

genericTypeDecoder : String -> String
genericTypeDecoder type_ =
  case type_ of
    "String" -> "Decode.string"
    "Int" -> "Decode.int"
    "Float" -> "Decode.float"
    "Bool" -> "Decode.bool"
    _ -> ""

genericTypeEncoder : String -> String
genericTypeEncoder type_ =
  case type_ of
    "String" -> "Encode.string"
    "Int" -> "Encode.int"
    "Float" -> "Encode.float"
    "Bool" -> "Encode.bool"
    _ -> ""

typedDecoder : List String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
typedDecoder moduleName type_ annotations =
  genericTypeDecoder type_

typedEncoder : List String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
typedEncoder moduleName type_ annotations =
  genericTypeEncoder type_

recordDecoder : Annotation.RecordDefinition -> String
recordDecoder definition =
  definition
  |> List.map recordFieldDecoder
  |> String.join "|> andMap "

recordEncoder : Annotation.RecordDefinition -> String
recordEncoder definition =
  definition
  |> List.map recordFieldEncoder
  |> String.join "\n, "

recordFieldDecoder : (String, (Range.Range, Annotation.TypeAnnotation)) -> String
recordFieldDecoder (name, (_, content)) =
  [ "Decode.field"
  , surroundByQuotes name
  , surroundByParen (typeAnnotationDecoder content)
  ]
  |> spaceJoin
  |> surroundByParen

recordFieldEncoder : (String, (Range.Range, Annotation.TypeAnnotation)) -> String
recordFieldEncoder (name, (_, content)) =
  [ surroundByQuotes name
  , surroundByParen (typeAnnotationEncoder content ++ " record." ++ name)
  ]
  |> String.join ", "
  |> surroundByParen

keepAliasDecl : Declaration.Declaration -> List Alias.TypeAlias
keepAliasDecl declaration =
  case declaration of
    Declaration.AliasDecl decl -> List.singleton decl
    _ -> []

findByName : String -> List Alias.TypeAlias -> Maybe Alias.TypeAlias
findByName name aliases =
  case aliases of
    [] -> Nothing
    hd :: tl ->
      if hd.name == name then
        Just hd
      else
        findByName name tl

extractRecord : Annotation.TypeAnnotation -> Maybe Annotation.RecordDefinition
extractRecord annotation =
  case annotation of
    Annotation.Record definition -> Just definition
    _ -> Nothing

surroundByQuotes : String -> String
surroundByQuotes value =
  "\"" ++ value ++ "\""

surroundByParen : String -> String
surroundByParen value =
  "(" ++ value ++ ")"

surroundByBrackets : String -> String
surroundByBrackets value =
  "[" ++ value ++ "]"

andMapFunction : String
andMapFunction =
  [ "andMap : Decoder a -> Decoder (a -> b) -> Decoder b"
  , "andMap = Decode.map2 (|>)"
  ]
  |> newlineJoin

camelize : String -> String
camelize value =
  value
  |> String.toList
  |> lowercaseFirst
  |> String.fromList

lowercaseFirst : List Char -> List Char
lowercaseFirst chars =
  case chars of
    hd :: tl -> Char.toLower hd :: tl
    [] -> []

indent : String -> String
indent = (++) " "

spaceJoin : List String -> String
spaceJoin = String.join " "

newlineJoin : List String -> String
newlineJoin = String.join "\n"

newline : String -> String
newline = (++) "\n"
