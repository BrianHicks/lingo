module Word (..) where

import String
import Effects exposing (Effects)
import Routing
import Signal
import Html exposing (Html)
import Html.Shorthand as H


-- MODEL


type Level
  = Unknown
  | New
  | SeenBefore
  | AlmostKnown
  | WellKnown
  | Ignored


defaultLevel : Level
defaultLevel =
  Unknown


type alias Model =
  { word : String
  , level : Level
  , meaning : Maybe String
  , example : Maybe String
  , exported : Bool
  }


init : String -> Model
init word =
  { word = word
  , level = defaultLevel
  , meaning = Nothing
  , example = Nothing
  , exported = False
  }



-- ACTION


type Action
  = SetLevel Level



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    SetLevel level ->
      ( { model | level = level }
      , Effects.none
      )



-- ROUTER


route : Routing.Model -> Signal.Address Action -> Model -> Html
route _ address model =
  view address model



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  H.section'
    { class = "word", id = "word-" ++ (String.toLower model.word) }
    [ H.h2_ model.word
    , H.p' { class = "level" } [ Html.text (toString model.level) ]
    , H.p' { class = "meaning" } [ Html.text (toString model.meaning) ]
    , H.p' { class = "example" } [ Html.text (toString model.example) ]
    ]



-- UTILITY


levelClass : Level -> String
levelClass =
  toString >> String.toLower
