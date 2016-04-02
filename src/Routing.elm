module Routing (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Signal
import String


-- MODEL


type alias Model =
  List String


type alias View a b =
  Signal.Address a -> b -> Html


init : Model
init = []


-- ACTION


type Action
  = PathChange String



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    PathChange path ->
      ( sanitize path, Effects.none )



-- VIEW


notFound : Html
notFound =
  Html.p [] [ Html.text "Not Found" ]



-- UTILITY


sanitize : String -> Model
sanitize path =
  path
    |> String.split "/"
    |> List.filter (\seg -> seg /= "" && seg /= "#")


serialize : Model -> String
serialize path =
  path |> String.join "/" |> String.append "#/"
