module Generator.Decoder exposing (..)

import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation
import Elm.Syntax.Range as Range

import String.Extra as String
import Aliases exposing (..)

generateAliasDecoderAndDeps : Alias.TypeAlias -> (String, List (ModuleName, String))
generateAliasDecoderAndDeps declaration =
  let decoderAndDeps = aliasDeclDecoderAndDeps declaration in
  Tuple.mapFirst (encloseInDecoderFunction declaration.name) decoderAndDeps

aliasDeclDecoderAndDeps : Alias.TypeAlias -> (String, List (ModuleName, String))
aliasDeclDecoderAndDeps { name, typeAnnotation } =
  typeAnnotation
  |> Tuple.second
  |> typeAnnotationDecoder
  |> Tuple.mapFirst (addDecoderStructure name)

typeAnnotationDecoder : Annotation.TypeAnnotation -> (String, List (ModuleName, String))
typeAnnotationDecoder typeAnnotation =
  case typeAnnotation of
    Annotation.Record definition -> recordDecoder definition
    Annotation.GenericType type_ -> genericTypeDecoder type_
    Annotation.Typed moduleName value annotations -> typedDecoder moduleName value annotations
    -- Annotation.Unit ->
    -- Annotation.Tupled annotations ->
    -- Annotation.GenericRecord name definition ->
    -- Annotation.FunctionTypeAnnotation annotation annotation ->
    _ -> ("", [])

recordDecoder : Annotation.RecordDefinition -> (String, List (ModuleName, String))
recordDecoder definition =
  definition
  |> List.map recordFieldDecoder
  |> flattenTuples
  |> Tuple.mapFirst (String.join "|> andMap ")

recordFieldDecoder : (String, (Range.Range, Annotation.TypeAnnotation)) -> (String, List (ModuleName, String))
recordFieldDecoder (name, (_, content)) =
  content
  |> typeAnnotationDecoder
  |> Tuple.mapFirst (recordFieldDecoderString name)

recordFieldDecoderString : String -> String -> String
recordFieldDecoderString name decoder =
    [ "Decode.field"
    , String.surroundByQuotes name
    , String.surroundByParen decoder
    ]
    |> String.spaceJoin
    |> String.surroundByParen

flattenTuples : List (String, List (ModuleName, String)) -> (List String, List (ModuleName, String))
flattenTuples = List.foldr concatDecoderFieldsAndKeepDeps ([], [])

concatDecoderFieldsAndKeepDeps
   : (String, List (ModuleName, String))
  -> (List String, List (ModuleName, String))
  -> (List String, List (ModuleName, String))
concatDecoderFieldsAndKeepDeps (content, deps) (accDecoder, accDeps) =
  (content :: accDecoder, accDeps ++ deps)

genericTypeDecoder : String -> (String, List (ModuleName, String))
genericTypeDecoder type_ =
  case type_ of
    "String" -> ("Decode.string", [])
    "Int" -> ("Decode.int", [])
    "Float" -> ("Decode.float", [])
    "Bool" -> ("Decode.bool", [])
    value -> ("decode" ++ value, [ ("", value) ])

typedDecoder : List String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> (String, List (ModuleName, String))
typedDecoder moduleName type_ annotations =
  genericTypeDecoder type_

addDecoderStructure : String -> String -> String
addDecoderStructure name decoder =
  [ [ "Decode.succeed", name, String.newline "|> andMap" ]
  , [ decoder ]
  ]
  |> List.map String.spaceJoin
  |> String.spaceJoin
  |> String.surroundByParen

encloseInDecoderFunction : String -> String -> String
encloseInDecoderFunction name decoder =
  let functionName = String.camelize name ++ "Decoder" in
  [ [ functionName, ": Decoder", name ]
  , [ functionName, "=" ]
  , [ String.indent decoder ]
  ]
  |> List.map String.spaceJoin
  |> String.newlineJoin
