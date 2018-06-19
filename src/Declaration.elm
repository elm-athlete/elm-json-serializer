module Declaration exposing (..)

import Elm.RawFile exposing (RawFile)
import Elm.Processing
import Elm.Syntax.Declaration as Declaration

getDeclarationByName : String -> RawFile -> Maybe Declaration.Declaration
getDeclarationByName name rawFile =
  Elm.Processing.process Elm.Processing.init rawFile
  |> .declarations
  |> List.map Tuple.second
  |> List.filter isAliasOrType
  |> findDeclarationByName name

isAliasOrType : Declaration.Declaration -> Bool
isAliasOrType declaration =
  case declaration of
    Declaration.AliasDecl decl -> True
    Declaration.TypeDecl decl -> True
    _ -> False

findDeclarationByName
   : String
  -> List Declaration.Declaration
  -> Maybe Declaration.Declaration
findDeclarationByName name declarations =
  case declarations of
    Declaration.AliasDecl decl :: tl ->
      if decl.name == name then
        Just (Declaration.AliasDecl decl)
      else
        findDeclarationByName name tl
    Declaration.TypeDecl decl :: tl ->
      if decl.name == name then
        Just (Declaration.TypeDecl decl)
      else
        findDeclarationByName name tl
    _ :: tl ->
      findDeclarationByName name tl
    [] ->
      Nothing
