module Language (..) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import Html exposing (Html)
import Routing
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
  = SourceAction String Source.Action



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    SourceAction slug action ->
      case sourceBySlug slug model of
        Nothing ->
          ( model, Effects.none )

        Just target ->
          let
            ( source, fx ) =
              Source.update action target
          in
            ( model |> addSource source
            , Effects.map (SourceAction slug) fx
            )



-- ROUTER


route : Routing.Model -> Signal.Address Action -> Model -> Html
route path address model =
  case path of
    [] ->
      view address model

    "sources" :: slug :: rest ->
      case sourceBySlug slug model of
        Nothing ->
          Routing.notFound

        Just source ->
          Source.route rest (Signal.forwardTo address (SourceAction slug)) source

    _ ->
      Routing.notFound



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let
    sources =
      model.sources
        |> Dict.toList
        |> List.map
            (\( slug, source ) ->
              Source.summaryView
                (Signal.forwardTo address (SourceAction slug))
                ( "TODO", source )
            )

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
  { model | sources = Dict.insert (Source.slug source) source model.sources }


sourceBySlug : String -> Model -> Maybe Source.Model
sourceBySlug slug model =
  Dict.get slug model.sources
