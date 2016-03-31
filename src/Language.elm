module Language (..) where

import Html exposing (Html)
import Effects exposing (Effects)

-- MODEL

type alias Language = { name : String }

type alias Model = List Language

init : Model
init = [ { name = "Spanish" } ]

-- ACTION

type Action = AddLanguage String

-- UPDATE

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddLanguage name ->
      ( { name = name } :: model, Effects.none)

-- VIEW

languageView : Signal.Address Action -> Language -> Html
languageView address model =
  Html.li [] [ Html.text model.name ]

view : Signal.Address Action -> Model -> Html
view address model =
  Html.ul [] (List.map (languageView address) model)
