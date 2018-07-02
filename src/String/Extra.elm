module String.Extra
  exposing
    ( camelize
    , indent
    , spaceJoin
    , newline
    , newlineJoin
    , surroundByParen
    , surroundByQuotes
    , surroundByBrackets
    , fromInt
    )

import Char

camelize : String -> String
camelize value =
  value
  |> String.toList
  |> lowercaseFirst
  |> String.fromList

lowercaseFirst : List Char -> List Char
lowercaseFirst chars =
  case chars of
    hd :: tl -> Char.toLower hd :: tl
    [] -> []

indent : String -> String
indent = (++) " "

spaceJoin : List String -> String
spaceJoin = String.join " "

newlineJoin : List String -> String
newlineJoin = String.join "\n"

newline : String -> String
newline = (++) "\n"

surroundByQuotes : String -> String
surroundByQuotes value =
  "\"" ++ value ++ "\""

surroundByParen : String -> String
surroundByParen value =
  "(" ++ value ++ ")"

surroundByBrackets : String -> String
surroundByBrackets value =
  "[" ++ value ++ "]"

fromInt : Int -> String
fromInt = toString
