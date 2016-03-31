module Source (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Signal


-- MODEL


type alias Model =
  { title : String
  , text : String
  , source : Maybe String
  }
