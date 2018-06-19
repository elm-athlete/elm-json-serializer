module Generator.Module exposing (..)

import String.Extra as String
import Aliases exposing (..)

type DecoderEncoder
  = Decoder
  | Encoder

addModuleName : String -> DecoderEncoder -> String -> String
addModuleName name decoder content =
  generateFileContent name content <|
    case decoder of
      Decoder ->
        GenerationRequirements
          "Decoder"
          "Json.Decode as Decode"
          ([ andMapFunction
           , tupleThreeFunction
           , tupleFourFunction
           ]
           |> String.newlineJoin)
      Encoder ->
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
  , String.spaceJoin [ "import", name, "exposing (..)"]
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

andMapFunction : String
andMapFunction =
  [ "andMap : Decoder a -> Decoder (a -> b) -> Decoder b"
  , "andMap = Decode.map2 (|>)"
  ]
  |> String.newlineJoin

tupleThreeFunction : String
tupleThreeFunction =
  [ "tupleThree : a -> b -> c -> (a, b, c)"
  , "tupleThree a b c d = Decode.map3 (\\a b c -> (a, b, c))"
  ]
  |> String.newlineJoin

tupleFourFunction : String
tupleFourFunction =
  [ "tupleFour : a -> b -> c -> d -> (a, b, c, d)"
  , "tupleFour a b c d = Decode.map4 (\\a b c d -> (a, b, c, d))"
  ]
  |> String.newlineJoin
