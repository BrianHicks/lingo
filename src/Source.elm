module Source (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
import Regex
import Routing
import Signal
import String


-- MODEL


type alias Model =
  { title : String
  , text : String
  , source : Maybe String
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


summaryView : Signal.Address Action -> ( String, Model ) -> Html
summaryView address ( href, model ) =
  Html.li
    []
    [ Html.a
        [ Attributes.href href ]
        [ Html.h3 [] [ Html.text model.title ]
        ]
    , Html.p [] [ Html.text (String.left 100 model.text) ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
    []
    [ Html.h1 [] [ Html.text model.title ]
    , Html.p [] [ Html.text (model |> wordsInContext |> toString) ]
    ]



-- UTILITY


slug : Model -> String
slug model =
  model.title
    |> String.toLower
    |> Regex.replace Regex.All (Regex.regex "[^\\w\\d\\-\\s]") (\_ -> "")
    |> Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> "-")


wordsInContext : Model -> List ( String, String )
wordsInContext model =
  let
    begins =
      "[¡¿]?"

    word =
      "[a-zA-ZÀ-ÖØ-öø-ȳ]+"

    words =
      "(" ++ word ++ "\\s?)+"

    ends =
      "[\\.!\\?:;]"
  in
    model.text
      |> Regex.find Regex.All (Regex.regex (begins ++ words ++ ends))
      |> List.concatMap
          (\sentence ->
            sentence.match
              |> Regex.find Regex.All (Regex.regex word)
              |> List.map (\word -> ( word.match, sentence.match ))
          )
