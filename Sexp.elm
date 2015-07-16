module Sexp where

import Result
import String
import List

import Parser
import Parser exposing (Parser, (<*>), (<*), (<$>))
import Parser.Char

import Graphics.Element


type Sexp
  = SexpList (List Sexp)
  | SexpElement String

parse string =
  Result.toMaybe <| Parser.parse parser string

parser =
  Parser.recursively
    (\() ->
      Parser.or
        (SexpList <$>
          (Parser.Char.parenthesized
            <| Parser.optional
              (Parser.separatedBy parser
                (Parser.satisfy
                  <| \c -> (List.member c <| String.toList " \t\r\n\v\f")))
              []))
        (SexpElement << String.fromList <$>
          (Parser.some
            <| Parser.satisfy
              <| \c -> not (List.member c <| String.toList " \t\r\n\v\f()"))))

show sexp =
  case sexp of
    SexpElement string ->
      string
    SexpList sexps ->
      "(" ++ (String.join " " <| List.map show sexps) ++ ")"

test =
  [ parse "(() test)"   == Just (SexpList [ SexpList [], SexpElement "test" ])
  , parse "()"          == Just (SexpList [])
  , parse "(test)"      == Just (SexpList [ SexpElement "test" ])
  , show (SexpList [])  == "()"
  , show
      (SexpList
        [ SexpElement "test"
        ])              == "(test)"
  , show
    (SexpList
      [ SexpList []
      , SexpElement "test"
      ])                == "(() test)"
  ]

main : Graphics.Element.Element
main = Graphics.Element.show test
