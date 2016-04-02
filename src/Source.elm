module Source (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Signal
import String
import Regex


-- MODEL


type alias Model =
  { title : String
  , text : String
  , source : Maybe String
  }



-- ACTION


type Action
  = TODO



-- VIEW


summaryView : Signal.Address Action -> (String, Model) -> Html
summaryView address (slug, model) =
  Html.li
    []
    [ Html.h3 [] [ Html.text (model.title ++ " (" ++ slug ++ ")") ]
    , Html.p [] [ Html.text (String.left 100 model.text) ]
    ]



-- UTILITY


slug : Model -> String
slug model =
  model.title
    |> String.toLower
    |> Regex.replace Regex.All (Regex.regex "[^\\w\\d\\-\\s]") (\_ -> "")
    |> Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> "-")
