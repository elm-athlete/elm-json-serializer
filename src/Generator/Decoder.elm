module Generator.Decoder exposing (..)

import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation
import Elm.Syntax.Range as Range

import String.Extra as String
import Aliases exposing (..)
import Dependency exposing (Dependency(..))

generateAliasDecoderAndDeps : Alias.TypeAlias -> (String, List (Dependency, String))
generateAliasDecoderAndDeps declaration =
  let decoderAndDeps = aliasDeclDecoderAndDeps declaration in
  Tuple.mapFirst (encloseInDecoderFunction declaration.name) decoderAndDeps

aliasDeclDecoderAndDeps : Alias.TypeAlias -> (String, List (Dependency, String))
aliasDeclDecoderAndDeps { name, typeAnnotation } =
  typeAnnotation
  |> Tuple.second
  |> typeAnnotationDecoder
  |> Tuple.mapFirst (addDecoderPipelineStructure name)

typeAnnotationDecoder : Annotation.TypeAnnotation -> (String, List (Dependency, String))
typeAnnotationDecoder typeAnnotation =
  case typeAnnotation of
    Annotation.Record definition -> recordDecoder definition
    Annotation.GenericType type_ -> (genericTypeDecoder type_, [])
    Annotation.Typed moduleName value annotations -> typedDecoder moduleName value annotations
    -- Annotation.Unit ->
    Annotation.Tupled annotations -> tupledDecoder annotations
    -- Annotation.GenericRecord name definition ->
    -- Annotation.FunctionTypeAnnotation annotation annotation ->
    _ -> ("", [])

recordDecoder : Annotation.RecordDefinition -> (String, List (Dependency, String))
recordDecoder definition =
  definition
  |> List.map recordFieldDecoder
  |> flattenTuples
  |> Tuple.mapFirst (String.join "|> andMap ")

recordFieldDecoder
   : (String, (Range.Range, Annotation.TypeAnnotation))
  -> (String, List (Dependency, String))
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

flattenTuples : List (String, List (Dependency, String)) -> (List String, List (Dependency, String))
flattenTuples = List.foldr concatDecoderFieldsAndKeepDeps ([], [])

concatDecoderFieldsAndKeepDeps
   : (String, List (Dependency, String))
  -> (List String, List (Dependency, String))
  -> (List String, List (Dependency, String))
concatDecoderFieldsAndKeepDeps (content, deps) (accDecoder, accDeps) =
  (content :: accDecoder, accDeps ++ deps)

genericTypeDecoder : String -> String
genericTypeDecoder type_ =
  case type_ of
    "String" -> "Decode.string"
    "Int" -> "Decode.int"
    "Float" -> "Decode.float"
    "Bool" -> "Decode.bool"
    value -> "decode" ++ value

typedDecoder
   : List String
  -> String
  -> List (Range.Range, Annotation.TypeAnnotation)
  -> (String, List (Dependency, String))
typedDecoder moduleName type_ annotations =
  (genericTypeDecoder type_, dependencyIfNotGenericType moduleName type_)

dependencyIfNotGenericType moduleName type_ =
  case type_ of
    "String" -> []
    "Int" -> []
    "Float" -> []
    "Bool" -> []
    value -> [ (InModule (String.join "." moduleName), type_) ]

tupledDecoder : List (Range.Range, Annotation.TypeAnnotation) -> (String, List (Dependency, String))
tupledDecoder annotations =
  annotations
  |> List.map Tuple.second
  |> List.map typeAnnotationDecoder
  |> flattenTuples
  |> Tuple.mapFirst (List.indexedMap tupleFieldDecoder)
  |> Tuple.mapFirst (String.join "\n ")
  |> Tuple.mapFirst (addTupleMapper annotations)

tupleFieldDecoder : Int -> String -> String
tupleFieldDecoder index value =
  [ "Decode.field"
  , index
    |> String.fromInt
    |> String.surroundByQuotes
  , value
  ]
  |> String.spaceJoin
  |> String.surroundByParen

addTupleMapper : List annotations -> String -> String
addTupleMapper annotations =
  String.append
    (case List.length annotations of
      2 -> "Decode.map Tuple.pair"
      3 -> "Decode.map tupleThree"
      4 -> "Decode.map tupleFour"
      _ -> "We should add more cases here..."
    )

addDecoderPipelineStructure : String -> String -> String
addDecoderPipelineStructure name decoder =
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
