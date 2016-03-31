module Lingo (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
import Language
import Routing


-- MODEL


type alias Model =
  { location : Routing.Model
  , languages : Language.Model
  }


init : Model
init =
  { location = Just Routing.Languages
  , languages = Language.init
  }



-- ACTION


type Action
  = RoutingAction Routing.Action
  | LanguageAction Language.Action



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    RoutingAction action ->
      let
        ( location, fx ) =
          Routing.update action model.location
      in
        ( { model | location = location }, Effects.map RoutingAction fx )

    LanguageAction action ->
      let
        ( languages, fx ) =
          Language.update action model.languages
      in
        ( { model | languages = languages }, Effects.map LanguageAction fx )



-- VIEW


navItem : Routing.Location -> Html
navItem location =
  Html.li
    []
    [ Html.a
        [ Attributes.href (Routing.toPath location) ]
        [ Html.text (Routing.toName location) ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    content =
      case model.location of
        Nothing ->
          Html.p [] [ Html.text "Not Found" ]

        Just (Routing.Languages) ->
          Language.view (Signal.forwardTo address LanguageAction) model.languages
  in
    Html.div
      []
      [ model |> toString |> Html.text
      , Html.nav
          []
          [ Html.h2 [] [ Html.text "Navigation" ]
          , Html.ul
              []
              (List.map navItem [ Routing.Languages ])
          ]
      , Html.div
          []
          [ Html.h2 [] [ Html.text "Content" ]
          , content
          ]
      ]
