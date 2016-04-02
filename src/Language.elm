module Language (..) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import Html exposing (Html)
import Signal
import Source
import Word


-- MODEL


type alias Model =
  { name : String
  , sources : Dict String Source.Model
  , archivedSources : Dict String Source.Model
  , words : Dict String Word.Model
  }


init : String -> Model
init name =
  { name = name
  , sources = Dict.empty
  , archivedSources = Dict.empty
  , words = Dict.empty
  }



-- ACTION


type Action
  = SourceAction Source.Action



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( model, Effects.none )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let
    sourceSummary =
      Source.summaryView (Signal.forwardTo address SourceAction)

    sources =
      model.sources
        |> Dict.toList
        |> List.map sourceSummary

    sourceCount =
      model.sources |> Dict.size |> toString

    archivedCount =
      model.archivedSources |> Dict.size |> toString
  in
    Html.div
      []
      [ Html.h1 [] [ Html.text model.name ]
      , Html.h2 [] [ Html.text "Sources" ]
      , Html.p [] [ Html.text (sourceCount ++ " sources (" ++ archivedCount ++ " archived)") ]
      , Html.ul [] sources
      ]


-- UTILITY

addSource : Source.Model -> Model -> Model
addSource source model =
  { model | sources = Dict.insert (Source.slug source) source model.sources}

sourceBySlug : Model -> String -> Maybe Source.Model
sourceBySlug model source =
  Dict.get source model.sources
