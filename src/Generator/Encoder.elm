module Generator.Encoder exposing (..)

import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation
import Elm.Syntax.Range as Range

import String.Extra as String

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

generateAliasEncoder : Alias.TypeAlias -> String
generateAliasEncoder ({ name } as declaration) =
  let functionName = "encode" ++ name in
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
