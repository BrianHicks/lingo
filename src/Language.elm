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
  , sources : List Source.Model
  , archivedSources : List Source.Model
  , words : Dict String Word.Model
  }


init : String -> Model
init name =
  { name = name
  , sources = []
  , archivedSources = []
  , words = Dict.empty
  }



-- ACTION


type Action
  = TODO



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( model, Effects.none )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  Html.text model.name
