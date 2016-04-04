module Routing (..) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import Html exposing (Html)
import Regex
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
  , query :
      Dict String String
  }


type alias View a b =
  Signal.Address a -> b -> Html


init : Model
init =
  { below = [], above = [], query = Dict.empty }



-- ACTION


type Action
  = PathChange String



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    PathChange raw ->
      let
        new =
          case Regex.split (Regex.AtMost 1) (Regex.regex "\\?") raw of
            path::query::[] ->
              { model
                | query = parseQuery query
                , below = sanitize path
                , above = []
              }

            path::[] ->
              { model
                | below = sanitize path
                , above = []
                , query = Dict.empty
              }

            _ ->
              model

        fx =
          Effects.none
      in
        ( new, fx )



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


serialize : Model -> String
serialize model =
  model.above
    |> String.join "/"
    |> String.append "#/"


-- yes, I know that query strings can have duplicate keys. I'll implement it if
-- I need it.
parseQuery : String -> Dict String String
parseQuery raw =
  let
    keyValuer =
      \kv ->
        case String.split "=" kv of
          k::v::[] ->
            (k, v)
          k::[] ->
            (k, "")
          _ ->
            (kv, "")
  in
   raw
    |> String.split "&"
    |> List.map keyValuer
    |> Dict.fromList


popN : Int -> Model -> Model
popN number model =
  { model
    | below = List.drop number model.below
    , above = model.above ++ List.take number model.below
  }


pop : Model -> Model
pop =
  popN 1


here : Model -> String
here model =
  serialize model


below : String -> Model -> String
below location model =
  serialize { model | above = model.above ++ [ location ]}
