port module Main exposing (..)

import Json.Decode as Decode
import Elm.Parser
import Elm.RawFile exposing (RawFile)
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation

type alias Model =
  Int

type Msg
  = FromJs (String, String)

port fromJs : ((String, String) -> msg) -> Sub msg

main : Program () Model Msg
main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init flags =
  (0, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FromJs (value, type_) ->
      let print = Debug.log "value" value
          print_ = value |> Elm.Parser.parse |> Result.map (extractType type_) in
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  fromJs FromJs

extractType : String -> RawFile -> String
extractType type_ rawFile =
  Elm.Processing.process Elm.Processing.init rawFile
  |> .declarations
  |> List.map Tuple.second
  |> extractFromDeclaration type_

extractFromDeclaration : String -> List Elm.Syntax.Declaration.Declaration -> String
extractFromDeclaration type_ declarations =
  declarations
  |> List.concatMap keepAliasDecl
  |> findByName type_
  |> Maybe.map (.typeAnnotation >> Tuple.second)
  -- Maybe TypeAnnotation
  |> Debug.log "file"
  |> always ""

keepAliasDecl : Elm.Syntax.Declaration.Declaration -> List Elm.Syntax.TypeAlias.TypeAlias
keepAliasDecl declaration =
  case declaration of
    Elm.Syntax.Declaration.AliasDecl decl -> List.singleton decl
    _ -> []

findByName : String -> List Elm.Syntax.TypeAlias.TypeAlias -> Maybe Elm.Syntax.TypeAlias.TypeAlias
findByName name_ aliases =
  case aliases of
    [] -> Nothing
    hd :: tl ->
      if hd.name == name_ then
        Just hd
      else
        findByName name_ tl

extractRecord : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Maybe Elm.Syntax.TypeAnnotation.RecordDefinition
extractRecord annotation =
  case annotation of
    Elm.Syntax.TypeAnnotation.Record definition -> Just definition
    _ -> Nothing
