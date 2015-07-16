module Test where

import Graphics.Element

import Sexp exposing (Sexp(..), parse, show)


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
