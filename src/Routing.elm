module Routing (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Signal
import String


-- MODEL


type alias Model =
  { below :
      List String
      -- components to be navigated
  , above :
      List String
      -- components already navigated
  }


type alias View a b =
  Signal.Address a -> b -> Html


init : Model
init =
  { below = [], above = [] }



-- ACTION


type Action
  = PathChange String



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    PathChange path ->
      ( { below = sanitize path
        , above = []
        }
      , Effects.none
      )



-- VIEW


notFound : Html
notFound =
  Html.p [] [ Html.text "Not Found" ]



-- UTILITY


sanitize : String -> List String
sanitize path =
  path
    |> String.split "/"
    |> List.filter (\seg -> seg /= "" && seg /= "#")


serialize : List String -> String
serialize path =
  path
    |> String.join "/"
    |> String.append "#/"


popN : Int -> Model -> Model
popN number model =
  { below = List.drop number model.below
  , above = model.above ++ List.take number model.below
  }


pop : Model -> Model
pop =
  popN 1


here : Model -> String
here model =
  serialize model.above


below : String -> Model -> String
below location model =
  serialize (model.above ++ [ location ])
