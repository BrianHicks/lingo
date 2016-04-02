module Routing (..) where

import Effects exposing (Effects)
import String


-- MODEL


type Location
  = Languages
  | Language String
  | Source String String


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

        Language name ->
          "/languages/" ++ name

        Source language name ->
          "/languages/" ++ language ++ "/" ++ name
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

      [ "languages", name ] ->
        Just (Language name)

      [ "languages", language, name ] ->
        Just (Source language name)

      _ ->
        Nothing


toName : Location -> String
toName location =
  case location of
    Language name ->
      name

    Source _ name ->
      name

    _ ->
      toString location
