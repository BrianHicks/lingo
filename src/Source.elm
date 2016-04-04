module Source (..) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
import Regex
import Routing
import Signal
import String
import Word
import Utilities


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


route : Routing.Model -> Signal.Address Action -> Dict String Word.Model -> Model -> Html
route route address words model =
  view route address words model



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


activateWords : Routing.Model -> Dict String Word.Model -> String -> List Html
activateWords route savedWords text =
  let
    activator =
      \word ->
        let
          class =
            case Dict.get word savedWords of
              Nothing ->
                Word.levelClass Word.defaultLevel

              Just saved ->
                Word.levelClass saved.level

          href =
            Routing.withQuery "word" word route |> Routing.serialize
        in
          Html.a
            [ Attributes.href href
            , Attributes.class class
            ]
            [ Html.text word ]

    words =
      text
        |> Regex.find Regex.All wordsRe
        |> List.map .match
        |> List.map activator

    nonwords =
      text
        |> Regex.split Regex.All wordsRe
        |> List.map Html.text
  in
    Utilities.longestZip nonwords words


view : Routing.Model -> Signal.Address Action -> Dict String Word.Model -> Model -> Html
view route address words model =
  let
    link =
      case model.source of
        Nothing ->
          Html.text ""

        Just url ->
          Html.a [ Attributes.href url, Attributes.class "source-link" ] [ Html.text "(source)" ]
  in
    Html.div
      [ Attributes.class "source" ]
      ([ Html.h1
          []
          [ Html.text model.title, link ]
       ]
        ++ (model.text
              |> String.split "\n\n"
              |> List.map (\paragraph -> Html.p [] (activateWords route words paragraph))
           )
      )



-- UTILITY


slug : Model -> String
slug model =
  model.title
    |> String.toLower
    |> Regex.replace Regex.All (Regex.regex "[^\\w\\d\\-\\s]") (\_ -> "")
    |> Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> "-")


wordsRe : Regex.Regex
wordsRe =
  Regex.regex "[a-zA-ZÀ-ÖØ-öø-ȳ]+"
