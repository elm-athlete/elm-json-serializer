module Generator.Encoder exposing (..)

import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAlias as Alias
import Elm.Syntax.TypeAnnotation as Annotation
import Elm.Syntax.Range as Range

import String.Extra as String
import Shared

aliasDeclEncoder : String -> Alias.TypeAlias -> String
aliasDeclEncoder recordName { name, typeAnnotation } =
  [ "Encode.object"
  , typeAnnotation
    |> Tuple.second
    |> typeAnnotationEncoder recordName ""
    |> String.surroundByBrackets
  ]
  |> String.spaceJoin
  |> String.surroundByParen

generateAliasEncoder : Alias.TypeAlias -> String
generateAliasEncoder ({ name } as declaration) =
  let functionName = "encode" ++ name in
  [ [ functionName, ":", name, "-> Encode.Value" ]
  , [ functionName, "record =" ]
  , [ String.indent (aliasDeclEncoder "record" declaration) ]
  ]
  |> List.map String.spaceJoin
  |> String.newlineJoin

generateTypedEncoder : Type.Type -> String
generateTypedEncoder ({ name } as type_) =
  let functionName = "encode" ++ name in
  [ [ functionName, ":", name, "-> Encode.Value" ]
  , [ functionName, "record =" ]
  , [ String.indent (generateTypedEncoderHelp type_) ]
  ]
  |> List.map String.spaceJoin
  |> String.newlineJoin

generateTypedEncoderHelp : Type.Type -> String
generateTypedEncoderHelp { name, generics, constructors } =
  [ [ "case record of" ]
  , constructors
    |> List.map constructorEncoder
    |> List.map String.indent
    |> List.map String.indent
  ]
  |> List.map String.newlineJoin
  |> String.newlineJoin

caseConstructor : Type.ValueConstructor -> String
caseConstructor { name, arguments } =
  let fieldNameForArguments index _ = Shared.indexToFieldName index in
  [ name
  , arguments
    |> List.indexedMap fieldNameForArguments
    |> String.spaceJoin
  , "->"
  ]
  |> String.spaceJoin

constructorBodyEncoder : Type.ValueConstructor -> String
constructorBodyEncoder { name, arguments } =
  [ [ String.surroundByQuotes "type"
    , ","
    , "Encode.string"
    , String.surroundByQuotes name
    ]
    |> String.spaceJoin
    |> String.surroundByParen
    |> List.singleton
  , arguments
    |> List.indexedMap argumentEncoder
  ]
  |> List.concat
  |> String.join "\n    ,"
  |> String.surroundByBrackets

constructorEncoder : Type.ValueConstructor -> String
constructorEncoder ({ name, arguments } as type_) =
  [ "Encode.object"
  , constructorBodyEncoder type_
  ]
  |> List.map String.indent
  |> String.join "\n  "
  |> String.append (caseConstructor type_)

argumentEncoder : Int -> (Range.Range, Annotation.TypeAnnotation) -> String
argumentEncoder index (_, annotation) =
  let fieldName = Shared.indexToFieldName index in
  [ String.surroundByQuotes fieldName
  , encloseArgumentBody fieldName annotation
  ]
  |> String.join ", "
  |> String.surroundByParen

encloseArgumentBody : String -> Annotation.TypeAnnotation -> String
encloseArgumentBody fieldName annotation =
  let annotationEncoderBody = typeAnnotationEncoder fieldName "" annotation in
  String.spaceJoin <|
    case annotation of
      Annotation.Record definition ->
        [ "Encode.object", String.surroundByBrackets annotationEncoderBody ]
      Annotation.GenericType type_ -> [ annotationEncoderBody, fieldName ]
      Annotation.Typed moduleName value annotations -> [ annotationEncoderBody, fieldName ]
      _ -> []

typeAnnotationEncoder : String -> String -> Annotation.TypeAnnotation -> String
typeAnnotationEncoder recordName fieldName typeAnnotation =
  case typeAnnotation of
    Annotation.Record definition -> recordEncoder recordName definition
    Annotation.GenericType type_ -> genericTypeEncoder recordName fieldName type_ []
    Annotation.Typed moduleName value annotations -> typedEncoder recordName fieldName moduleName value annotations
    -- Annotation.Unit ->
    Annotation.Tupled annotations -> tupleEncoder recordName fieldName annotations
    -- Annotation.GenericRecord name definition ->
    -- Annotation.FunctionTypeAnnotation annotation annotation ->
    _ -> ""

tupleEncoder : String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
tupleEncoder recordName fieldName annotations =
  annotations
  |> List.map Tuple.second
  |> List.map (typeAnnotationEncoder recordName fieldName)
  |> List.indexedMap (tupleFieldEncoder recordName fieldName)
  |> String.join "\n, "
  |> String.surroundByBrackets
  |> String.append "Encode.object"

tupleFieldEncoder : String -> String -> Int -> String -> String
tupleFieldEncoder recordName fieldName index value =
  String.surroundByParen
    (String.surroundByQuotes (String.fromInt index) ++ ", " ++ value ++ String.surroundByParen ("Tuple." ++ (if index == 0 then "first" else "second") ++ " " ++ recordName ++ "." ++ fieldName))

genericTypeEncoder : String -> String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
genericTypeEncoder recordName fieldName type_ annotations =
  case type_ of
    "String" -> "Encode.string"
    "Int" -> "Encode.int"
    "Float" -> "Encode.float"
    "Bool" -> "Encode.bool"
    "List" ->
      annotationInGenericTypeEncoder recordName fieldName "Encode.list" annotations
    "Maybe" ->
      annotationInGenericTypeEncoder recordName fieldName "encodeMaybe" annotations
    "Array" ->
      annotationInGenericTypeEncoder recordName fieldName "Encode.array" annotations
    "Set" ->
      annotationInGenericTypeEncoder recordName fieldName "Encode.set" annotations
    value -> "encode" ++ value

annotationInGenericTypeEncoder : String -> String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
annotationInGenericTypeEncoder recordName fieldName encoder annotations =
  annotations
  |> List.map Tuple.second
  |> List.map (typeAnnotationEncoder recordName fieldName)
  |> String.spaceJoin
  |> String.surroundByParen
  |> String.append encoder

typedEncoder : String -> String -> List String -> String -> List (Range.Range, Annotation.TypeAnnotation) -> String
typedEncoder recordName fieldName moduleName type_ annotations =
  genericTypeEncoder recordName fieldName type_ annotations

recordEncoder : String -> Annotation.RecordDefinition -> String
recordEncoder recordName definition =
  definition
  |> List.map (recordFieldEncoder recordName)
  |> String.join "\n, "

isTuple : Annotation.TypeAnnotation -> Bool
isTuple annotation =
  case annotation of
    Annotation.Tupled _ -> True
    _ -> False

recordFieldEncoder : String -> (String, (Range.Range, Annotation.TypeAnnotation)) -> String
recordFieldEncoder recordName (name, (_, content)) =
  [ String.surroundByQuotes name
  , String.surroundByParen ((typeAnnotationEncoder recordName name content) ++ (if isTuple content then "" else " " ++ recordName ++ "." ++ name))
  ]
  |> String.join ", "
  |> String.surroundByParen
