module Generator.Module exposing (..)

import Dict exposing (Dict)
import Elm.RawFile exposing (RawFile)

import String.Extra as String
import Aliases exposing (..)
import Dependency exposing (Dependency(..))
import Declaration
import Elm.Syntax.Declaration as Declaration
import Generator.Static

type DecoderEncoder
  = Decoder
  | Encoder

addModuleName
   : Dict ModuleName RawFile
  -> List (Dependency, String)
  -> ModuleName
  -> DecoderEncoder
  -> String
  -> String
addModuleName rawFiles dependencies moduleName decoder content =
  let imports = dependencies
                |> List.concatMap (generateImportsFromDeps rawFiles moduleName decoder)
                |> String.newlineJoin in
  generateFileContent moduleName imports content <|
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

generateImportsFromDeps
   : Dict ModuleName RawFile
  -> ModuleName
  -> DecoderEncoder
  -> (Dependency, String)
  -> List String
generateImportsFromDeps rawFiles moduleName decoder (dependency, typeName) =
  case dependency of
    InModule name ->
      case name == moduleName of
        True -> []
        False -> importDependency name decoder
    InOneOf names ->
      names
      |> List.map (\name -> (name, Dict.get name rawFiles))
      |> List.map (getDeclarationsInRawFiles typeName)
      |> List.concatMap (validDeclarationToImport decoder)

getDeclarationsInRawFiles
   : String
  -> (ModuleName, Maybe RawFile)
  -> (ModuleName, Maybe Declaration.Declaration)
getDeclarationsInRawFiles typeName =
  Tuple.mapSecond
    (Maybe.andThen
      (Declaration.getDeclarationByName typeName)
    )

validDeclarationToImport
   : DecoderEncoder
  -> (ModuleName, Maybe Declaration.Declaration)
  -> List String
validDeclarationToImport decoder (name, declaration) =
  case declaration of
    Nothing -> []
    Just _ -> importDependency name decoder

importDependency : ModuleName -> DecoderEncoder -> List String
importDependency name decoder =
  [ "import"
  , name ++ "." ++ case decoder of
    Decoder -> "Decoder"
    Encoder -> "Encoder"
  , "exposing (..)"
  ]
  |> String.spaceJoin
  |> List.singleton

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
