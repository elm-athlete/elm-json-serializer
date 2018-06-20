module Generator.Decoder exposing (..)

import Elm.Syntax.Type as Type
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

generateTypedDecoderAndDeps : Type.Type -> (String, List (Dependency, String))
generateTypedDecoderAndDeps type_ =
  let decoderAndDeps = typedDeclDecoderAndDeps type_ in
  Tuple.mapFirst (encloseInDecoderFunction type_.name) decoderAndDeps

aliasDeclDecoderAndDeps : Alias.TypeAlias -> (String, List (Dependency, String))
aliasDeclDecoderAndDeps { name, typeAnnotation } =
  typeAnnotation
  |> Tuple.second
  |> typeAnnotationDecoder
  |> Tuple.mapFirst (addDecoderPipelineStructure name)

typedDeclDecoderAndDeps : Type.Type -> (String, List (Dependency, String))
typedDeclDecoderAndDeps { name, generics, constructors } =
  let (body, dependencies) = unionConstructor constructors in
  ( [ baseUnionDecoder name
    , unionConstructorName name
    , body
    ]
    |> String.newlineJoin
  , dependencies
  )

unionConstructorName : String -> String
unionConstructorName name =
  let functionName = "as" ++ name ++ "Constructor" in
  [ functionName ++ " : Decoder " ++ name
  , functionName ++ " value ="
  ]
  |> String.newlineJoin

unionConstructor : List Type.ValueConstructor -> (String, List (Dependency, String))
unionConstructor constructors =
  let constructs = constructors
                   |> List.map constructorDecoder
                   |> List.map (Tuple.mapFirst String.indent)
      constructorCases = constructs
                         |> List.map Tuple.first
                         |> String.newlineJoin
      casePlaceholder = [ "case value of"
                        , constructorCases
                        , String.indent "_ -> Decode.fail"
                        ] in
  ( String.newlineJoin casePlaceholder
  , List.concatMap Tuple.second constructs
  )

constructorDecoder : Type.ValueConstructor -> (String, List (Dependency, String))
constructorDecoder { name, arguments } =
  let decoders = arguments
                 |> List.indexedMap argumentDecoder
                 |> List.map (Tuple.mapFirst String.surroundByParen) in
  ( [ decoderHeader name arguments
    , decoders
      |> List.map Tuple.first
      |> String.join "|> andMap"
      |> String.indent
      |> String.indent
    ]
    |> String.newlineJoin
  , List.concatMap Tuple.second decoders
  )

decoderHeader : String -> List a -> String
decoderHeader name arguments =
  [ String.surroundByQuotes name
  , "-> Decode.succeed"
  , name
  , if List.length arguments > 0 then
      "|> andMap"
    else
      ""
  ]
  |> String.spaceJoin

argumentDecoder
   : Int
  -> (Range.Range, Annotation.TypeAnnotation)
  -> (String, List (Dependency, String))
argumentDecoder index (_, annotation) =
  annotation
  |> typeAnnotationDecoder
  |> Tuple.mapFirst (putDecodeIndexedField index annotation)

putDecodeIndexedField : Int -> Annotation.TypeAnnotation -> String -> String
putDecodeIndexedField index annotation decoder =
  [ decodeIndexedField index
  , putRecordBaseIfNeeded annotation decoder
  ]
  |> String.spaceJoin

decodeIndexedField : Int -> String
decodeIndexedField index =
  [ "Decode.field"
  , index
    |> indexToFieldName
    |> String.surroundByQuotes
  ]
  |> String.spaceJoin

putRecordBaseIfNeeded : Annotation.TypeAnnotation -> String -> String
putRecordBaseIfNeeded annotation decoder =
  case annotation of
    Annotation.Record definition ->
      decoder
      |> String.append
        ("Decode.succeed " ++ anonymousRecord definition ++ "\n|> andMap")
      |> String.surroundByParen
    _ ->
      decoder

anonymousRecord : Annotation.RecordDefinition -> String
anonymousRecord definition =
  let lambdaArgs = definition
                   |> List.map Tuple.first
                   |> String.spaceJoin
      lambdaBodyArg (name, _) = name ++ " = " ++ name
      lambdaBody = definition
                   |> List.map lambdaBodyArg
                   |> String.join ", " in
  [ "\\", lambdaArgs, "->", "{", lambdaBody, "}" ]
  |> String.join ""
  |> String.surroundByParen

indexToFieldName : Int -> String
indexToFieldName index =
  case index of
    0 -> "first"
    1 -> "second"
    2 -> "third"
    3 -> "fourth"
    4 -> "fifth"
    5 -> "sixth"
    6 -> "seventh"
    7 -> "eigth"
    8 -> "nineth"
    10 -> "tenth"
    _ -> "WOW! You should reduce your number of arguments, or make a PR! 😄"

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

flattenTuples
   : List (String, List (Dependency, String))
  -> (List String, List (Dependency, String))
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

tupledDecoder
   : List (Range.Range, Annotation.TypeAnnotation)
  -> (String, List (Dependency, String))
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
  let functionName = "decode" ++ name in
  [ [ functionName, ": Decoder", name ]
  , [ functionName, "=" ]
  , [ String.indent decoder ]
  ]
  |> List.map String.spaceJoin
  |> String.newlineJoin

baseUnionDecoder : String -> String
baseUnionDecoder name =
  [ "Decode.andThen" ++ " as" ++ name ++ "Constructor"
  , [ "Decode.field"
    , String.surroundByQuotes "type"
    , "Decode.string"
    ]
    |> String.spaceJoin
    |> String.surroundByParen
  ]
  |> String.join "\n "
