module Sexp where

{-| Parses S-expressions.

@docs parse, show

-}

import Result
import String
import List

import Parser
import Parser exposing (Parser, (<*>), (<*), (<$>))
import Parser.Char


type Sexp
  = SexpList (List Sexp)
  | SexpElement String

{-| Parses an S-expression -}
parse : String -> Maybe Sexp
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

{-| Shows an S-expression -}
show : Sexp -> String
show sexp =
  case sexp of
    SexpElement string ->
      string
    SexpList sexps ->
      "(" ++ (String.join " " <| List.map show sexps) ++ ")"
