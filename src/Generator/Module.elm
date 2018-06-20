module Generator.Module exposing (..)

import Dict exposing (Dict)
import Elm.RawFile exposing (RawFile)

import String.Extra as String
import Aliases exposing (..)
import Dependency exposing (Dependency(..))
import Declaration
import Generator.Static

type DecoderEncoder
  = Decoder
  | Encoder

addModuleName : Dict ModuleName RawFile -> List (Dependency, String) -> ModuleName -> DecoderEncoder -> String -> String
addModuleName rawFiles dependencies moduleName decoder content =
  let imports = List.concatMap (generateImportsFromDeps rawFiles moduleName decoder) dependencies in
  generateFileContent moduleName (String.newlineJoin imports) content <|
    case decoder of
      Decoder ->
        GenerationRequirements
          "Decoder"
          "Json.Decode as Decode exposing (Decoder)"
      Encoder ->
        GenerationRequirements
          "Encoder"
          "Json.Encode as Encode"

type alias GenerationRequirements =
  { moduleNamespace : String
  , imported : String
  }

generateImportsFromDeps : Dict ModuleName RawFile -> ModuleName -> DecoderEncoder -> (Dependency, String) -> List String
generateImportsFromDeps rawFiles moduleName decoder (dependency, typeName) =
  case dependency of
    InModule name ->
      if name == moduleName then
        []
      else
        [ "import"
        , name ++ "." ++ case decoder of
          Decoder -> "Decoder"
          Encoder -> "Encoder"
        , "exposing (..)"
        ]
        |> String.spaceJoin
        |> List.singleton
    InOneOf names ->
      names
      |> List.map (\name -> (name, Dict.get name rawFiles))
      |> List.map (\(name, maybeRawFile) -> (name, Maybe.andThen (Declaration.getDeclarationByName typeName) maybeRawFile))
      |> List.concatMap (\(name, declaration) ->
        case declaration of
          Nothing -> []
          Just _ ->
            [ "import"
            , name ++ "." ++ case decoder of
              Decoder -> "Decoder"
              Encoder -> "Encoder"
            , "exposing (..)"
            ]
            |> String.spaceJoin
            |> List.singleton)

generateFileContent : ModuleName -> String -> String -> GenerationRequirements -> String
generateFileContent moduleName imports content { moduleNamespace, imported } =
  [ moduleGeneration moduleName moduleNamespace
  , String.spaceJoin [ "import", imported ]
  , String.spaceJoin [ "import", moduleName, "exposing (..)"]
  , String.spaceJoin [ "import", Generator.Static.moduleName, "exposing (..)" ]
  , imports
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
