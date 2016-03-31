module Routing (..) where

import Effects exposing (Effects)
import String


-- MODEL


type Location
  = Languages


type alias Model =
  Maybe Location



-- ACTION


type Action
  = PathChange String



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    PathChange path ->
      ( toLocation path, Effects.none )



-- UTILITY


toPath : Location -> String
toPath location =
  let
    path =
      case location of
        Languages ->
          "/languages"
  in
    "#" ++ path


toLocation : String -> Maybe Location
toLocation path =
  let
    segments =
      path
        |> String.split "/"
        |> List.filter (\seg -> seg /= "" && seg /= "#")
  in
    case segments of
      [ "languages" ] ->
        Just Languages

      _ ->
        Nothing


toName : Location -> String
toName location =
  toString location
