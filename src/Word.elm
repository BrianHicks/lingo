module Word (..) where

import String
import Effects exposing (Effects)
import Routing
import Signal
import Html exposing (Html)
import Html.Attributes as Attributes


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
  = TODO



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( model, Effects.none )



-- ROUTER


route : Routing.Model -> Signal.Address Action -> Model -> Html
route _ address model =
  view address model



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
    [ Attributes.class "word" ]
    [ Html.h2 [] [ Html.text model.word ]
    , Html.p [ Attributes.class "level" ] [ Html.text (toString model.level) ]
    , Html.p [ Attributes.class "meaning" ] [ Html.text (toString model.meaning) ]
    , Html.p [ Attributes.class "example" ] [ Html.text (toString model.example) ]
    ]



-- UTILITY


levelClass : Level -> String
levelClass =
  toString >> String.toLower
