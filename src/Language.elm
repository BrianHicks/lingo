module Language (..) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
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
  | WordAction String Word.Action



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

    WordAction raw action ->
      let
        word =
          case Dict.get raw model.words of
            Nothing ->
              Word.init raw
            Just saved ->
              saved

        ( word', fx ) =
          Word.update action word
      in
        ( model |> addWord word', Effects.map (WordAction raw) fx )




-- ROUTER


route : Routing.Model -> Signal.Address Action -> Model -> Html
route path address model =
  case path.below of
    [] ->
      view path address model

    "sources" :: slug :: _ ->
      let
        sourceContent =
          case sourceBySlug slug model of
            Nothing ->
              Routing.notFound

            Just source ->
              Source.route (Routing.popN 2 path) (Signal.forwardTo address (SourceAction slug)) model.words source

        word =
          case Dict.get "word" path.query of
            Nothing ->
              Nothing

            Just selected ->
              case Dict.get selected model.words of
                Nothing ->
                  Just (Word.init selected)

                Just saved ->
                  Just saved

        wordContent =
          case word of
            Nothing ->
              Html.div [ Attributes.class "empty" ] [ ]
            Just selected ->
              Word.route (Routing.popN 2 path) (Signal.forwardTo address (WordAction slug)) selected

      in
        Html.div [] [ wordContent, sourceContent ]

    _ ->
      Routing.notFound



-- VIEW


view : Routing.Model -> Signal.Address Action -> Model -> Html
view path address model =
  let
    sources =
      model.sources
        |> Dict.toList
        |> List.map
            (\( slug, source ) ->
              Source.summaryView
                (Signal.forwardTo address (SourceAction slug))
                ( path |> Routing.below ("sources/" ++ slug) |> Routing.serialize, source )
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


addWord : Word.Model -> Model -> Model
addWord word model =
  { model | words = Dict.insert word.word word model.words }


sourceBySlug : String -> Model -> Maybe Source.Model
sourceBySlug slug model =
  Dict.get slug model.sources
