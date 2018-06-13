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
    FromJs (value, type_) ->
      let print = Debug.log "value" value
          print_ = value |> Elm.Parser.parse |> Result.map (extractType type_) in
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  fromJs FromJs

extractType : String -> RawFile -> String
extractType type_ rawFile =
  Elm.Processing.process Elm.Processing.init rawFile
  |> .declarations
  |> List.map Tuple.second
  |> extractFromDeclaration type_

extractFromDeclaration : String -> List Declaration.Declaration -> String
extractFromDeclaration type_ declarations =
  declarations
  |> List.concatMap keepAliasDecl
  |> findByName type_
  |> Maybe.map convertAliasDecl
  |> Debug.log "file"
  |> Maybe.withDefault ""

convertAliasDecl : Alias.TypeAlias -> String
convertAliasDecl { name, typeAnnotation } =
  [ String.join " " [ "Decode.succeed", name, "|> andMap " ]
  , typeAnnotation
    |> Tuple.second
    |> convertTypeAnnotation
  ]
  |> String.join ""
  |> surroundByParen

convertTypeAnnotation : Annotation.TypeAnnotation -> String
convertTypeAnnotation typeAnnotation =
  case typeAnnotation of
    Annotation.Record definition -> convertRecord definition
    Annotation.GenericType type_ -> convertGenericType type_
    Annotation.Typed moduleName value annotations -> convertTyped moduleName value annotations
    -- Annotation.Unit ->
    -- Annotation.Tupled annotations ->
    -- Annotation.GenericRecord name definition ->
    -- Annotation.FunctionTypeAnnotation annotation annotation ->
    _ -> ""

convertGenericType : String -> String
convertGenericType type_ =
  case type_ of
    "String" -> "Decode.string"
    "Int" -> "Decode.int"
    "Float" -> "Decode.float"
    "Bool" -> "Decode.bool"
    _ -> ""

convertTyped : List String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
convertTyped moduleName type_ annotations =
  convertGenericType type_

convertRecord : Annotation.RecordDefinition -> String
convertRecord definition =
  definition
  |> List.map convertRecordField
  |> String.join "|> andMap "

convertRecordField : (String, (Range.Range, Annotation.TypeAnnotation)) -> String
convertRecordField (name, (_, content)) =
  [ "Decode.field"
  , surroundByQuotes name
  , surroundByParen (convertTypeAnnotation content)
  ]
  |> String.join " "
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

andMapFunction : String
andMapFunction =
  "map2 (|>)"
