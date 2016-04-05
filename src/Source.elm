module Source (..) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Shorthand as H
import Regex
import Routing
import Signal
import String
import Utilities
import Word


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
  H.li_
    [ Html.h2 [] [ H.a_ href model.title ]
    , H.p_ [ Html.text (String.left 100 model.text) ]
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
          H.a'
            { class = class
            , href = href
            }
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
          H.a' { class = "source-link", href = url } [ Html.text "(source)" ]
  in
    H.section'
      { class = "source", id = "source-" ++ (slug model) }
      ([ Html.h1
          []
          [ Html.text model.title, link ]
       ]
        ++ (model.text
              |> String.split "\n\n"
              |> List.map (activateWords route words)
              |> List.map H.p_
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
