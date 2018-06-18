module Generator.Module exposing (..)

import String.Extra as String
import Aliases exposing (..)

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

andMapFunction : String
andMapFunction =
  [ "andMap : Decoder a -> Decoder (a -> b) -> Decoder b"
  , "andMap = Decode.map2 (|>)"
  ]
  |> String.newlineJoin
