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
port toJs : (String, String) -> Cmd msg

type alias ReturnType =
  { decoder : String
  , encoder : String
  }

returnToTuple : ReturnType -> (String, String)
returnToTuple { decoder, encoder } =
  (decoder, encoder)

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
      ( model
      , value
        |> Elm.Parser.parse
        |> Result.map (extractType type_)
        |> Result.map returnToTuple
        |> Result.map toJs
        |> Result.withDefault Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  fromJs FromJs

extractType : String -> RawFile -> ReturnType
extractType type_ rawFile =
  Elm.Processing.process Elm.Processing.init rawFile
  |> .declarations
  |> List.map Tuple.second
  |> extractFromDeclaration type_

extractFromDeclaration : String -> List Declaration.Declaration -> ReturnType
extractFromDeclaration type_ declarations =
  let declaration = declarations
                    |> List.concatMap keepAliasDecl
                    |> findByName type_
      decoderTypeAlias = Maybe.map aliasDeclDecoder declaration
      encoderTypeAlias = Maybe.map aliasDeclEncoder declaration in
  ReturnType
    (Maybe.withDefault "" decoderTypeAlias)
    (Maybe.withDefault "" encoderTypeAlias)

aliasDeclEncoder : Alias.TypeAlias -> String
aliasDeclEncoder { name, typeAnnotation } =
  ""

aliasDeclDecoder : Alias.TypeAlias -> String
aliasDeclDecoder { name, typeAnnotation } =
  [ String.join " " [ "Decode.succeed", name, "|> andMap " ]
  , typeAnnotation
    |> Tuple.second
    |> typeAnnotationDecoder
  ]
  |> String.join ""
  |> surroundByParen

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

genericTypeDecoder : String -> String
genericTypeDecoder type_ =
  case type_ of
    "String" -> "Decode.string"
    "Int" -> "Decode.int"
    "Float" -> "Decode.float"
    "Bool" -> "Decode.bool"
    _ -> ""

typedDecoder : List String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
typedDecoder moduleName type_ annotations =
  genericTypeDecoder type_

recordDecoder : Annotation.RecordDefinition -> String
recordDecoder definition =
  definition
  |> List.map recordFieldDecoder
  |> String.join "|> andMap "

recordFieldDecoder : (String, (Range.Range, Annotation.TypeAnnotation)) -> String
recordFieldDecoder (name, (_, content)) =
  [ "Decode.field"
  , surroundByQuotes name
  , surroundByParen (typeAnnotationDecoder content)
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
