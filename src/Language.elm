module Language (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Signal


-- MODEL


type alias Model =
  { name : String }



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
