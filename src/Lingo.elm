module Lingo (..) where

import Html exposing (Html)
import Effects exposing (Effects)

import Language

-- MODEL

type alias Model = { languages : Language.Model }

init : Model
init = { languages = Language.init }

-- ACTION

type Action = LanguageAction Language.Action

-- UPDATE

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    LanguageAction action ->
      let
        (languages, fx) = Language.update action model.languages
      in
        ({ model | languages = languages }, Effects.map LanguageAction fx)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Language.view (Signal.forwardTo address LanguageAction) model.languages
