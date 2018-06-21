module Generator.Static exposing (..)

import String.Extra as String

moduleName : String
moduleName = "Serializer.Json.Extra"

jsonDecodeExtra : String
jsonDecodeExtra =
  [ "module " ++ moduleName ++ " exposing (..)"
  , "import Json.Decode as Decode exposing (Decoder)"
  , "import Json.Encode as Encode"
  , andMapFunction
  , tupleThreeFunction
  , tupleFourFunction
  , encodeMaybeFunction
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

encodeMaybeFunction : String
encodeMaybeFunction =
  [ "encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value"
  , "encodeMaybe mapper field ="
  , "  Maybe.withDefault Encode.null (Maybe.map mapper field)"
  ]
  |> String.newlineJoin
