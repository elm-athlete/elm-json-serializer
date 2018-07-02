module Dependency exposing (..)

import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Base as Base
import Elm.Syntax.Module as Module
import Elm.Syntax.Exposing as Exposing

import Shared exposing (..)
import Declaration

type Dependency
  = InModule ModuleName
  | InOneOf (List ModuleName)

type alias DecodersEncodersDeps =
  { decoder : String
  , encoder : String
  , dependencies : List (Dependency, String)
  }

fetchDependencies
   : ModuleName
  -> RawFile
  -> DecodersEncodersDeps
  -> DecodersEncodersDeps
fetchDependencies moduleName rawFile ({ dependencies } as store) =
  dependencies
  |> List.map (checkIfDependencyInFile moduleName rawFile)
  |> setDependenciesIn store

setDependenciesIn
   : DecodersEncodersDeps
  -> List (Dependency, String)
  -> DecodersEncodersDeps
setDependenciesIn store deps =
  { store | dependencies = deps }

checkIfDependencyInFile
   : ModuleName
  -> RawFile
  -> (Dependency, String)
  -> (Dependency, String)
checkIfDependencyInFile moduleName rawFile (dependency, typeName) =
  let imports = Elm.RawFile.imports rawFile in
  case dependency of
    InModule "" ->
      case Declaration.getDeclarationByName typeName rawFile of
        Nothing ->
          ( imports
            |> List.foldr (keepExposedNameOrExposingAll typeName) (False, [])
            |> Tuple.second
            |> List.map importToModuleName
            |> moduleNameToDependency
          , typeName
          )
        Just _ -> (InModule moduleName, typeName)
    InModule dependencyModule ->
      let fullModuleName = String.split "." dependencyModule in
      ( imports
        |> List.filter (isSameModuleName fullModuleName)
        |> List.map importToModuleName
        |> moduleNameToDependency
      , typeName
      )
    _ ->
      (dependency, typeName)

keepExposedNameOrExposingAll
   : String
  -> Module.Import
  -> (Bool, List Module.Import)
  -> (Bool, List Module.Import)
keepExposedNameOrExposingAll name ({ exposingList } as import_) (found, imports) =
  if found then
    (found, imports)
  else
    case exposingList of
      Nothing -> (False, imports)
      Just exposing_ ->
        case exposing_ of
          Exposing.All _ -> (False, import_ :: imports)
          Exposing.Explicit topLevelExpose ->
            topLevelExpose
            |> List.map Tuple.second
            |> List.map (isSomeTypeOrAliasExposed name)
            |> isSomeFound import_ imports False

type MaybeFound
  = Found
  | NotFound
  | CouldBeFound

isSomeFound
   : Module.Import
  -> List Module.Import
  -> Bool
  -> List MaybeFound
  -> (Bool, List Module.Import)
isSomeFound import_ imports onlyFound isFounds =
  case isFounds of
    [] -> (False, imports)
    Found :: tl -> (True, [ import_ ])
    NotFound :: tl -> isSomeFound import_ imports onlyFound tl
    CouldBeFound :: tl ->
      if onlyFound then
        isSomeFound import_ imports onlyFound tl
      else
        isSomeFound import_ (import_ :: imports) True tl

isSomeTypeOrAliasExposed : String -> Exposing.TopLevelExpose -> MaybeFound
isSomeTypeOrAliasExposed typeName topLevelExpose =
  case topLevelExpose of
    Exposing.TypeOrAliasExpose name ->
      if typeName == name then
        Found
      else
        NotFound
    Exposing.TypeExpose { name, constructors } ->
      if typeName == name then
        Found
      else
        case constructors of
          Nothing -> NotFound
          Just _ -> CouldBeFound
    _ ->
      NotFound

isSameModuleName : Base.ModuleName -> Module.Import -> Bool
isSameModuleName moduleName_ { moduleName, moduleAlias } =
  moduleName_ == moduleName || Just moduleName_ == moduleAlias

importToModuleName : Module.Import -> ModuleName
importToModuleName { moduleName } =
  String.join "." moduleName

moduleNameToDependency : List ModuleName -> Dependency
moduleNameToDependency moduleNames =
  if List.length moduleNames == 1 then
    moduleNames
    |> List.head
    |> Maybe.withDefault ""
    |> InModule
  else
    InOneOf moduleNames
