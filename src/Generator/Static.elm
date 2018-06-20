module Generator.Static exposing (..)

import String.Extra as String

moduleName : String
moduleName = "Serializer.Json.Decode.Extra"

jsonDecodeExtra : String
jsonDecodeExtra =
  [ "module " ++ moduleName ++ " exposing (..)"
  , "import Json.Decode as Decode exposing (Decoder)"
  , andMapFunction
  , tupleThreeFunction
  , tupleFourFunction
  ]
  |> String.newlineJoin

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
