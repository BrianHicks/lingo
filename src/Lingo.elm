module Lingo (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
import Language
import Languages
import Routing


-- MODEL


type alias Model =
  { location : Routing.Model
  , languages : Languages.Model
  }


init : Model
init =
  { location = Just Routing.Languages
  , languages = Languages.init
  }



-- ACTION


type Action
  = RoutingAction Routing.Action
  | LanguagesAction Languages.Action
  | LanguageAction String Language.Action



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

    LanguagesAction action ->
      let
        ( languages, fx ) =
          Languages.update action model.languages
      in
        ( { model | languages = languages }, Effects.map LanguagesAction fx )

    LanguageAction name action ->
      case Languages.byName model.languages name of
        Nothing ->
          ( model, Effects.none )

        Just target ->
          let
            ( language, fx ) =
              Language.update action target

            languages =
              Languages.updateIn model.languages language
          in
            ( { model | languages = languages }, Effects.map (LanguageAction name) fx )



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
    notFound =
      Html.p [] [ Html.text "Not Found" ]

    content =
      case model.location of
        Nothing ->
          notFound

        Just (Routing.Languages) ->
          Languages.view (Signal.forwardTo address LanguagesAction) model.languages

        Just (Routing.Language name) ->
          case Languages.byName model.languages name of
            Nothing ->
              notFound

            Just language ->
              Language.view (Signal.forwardTo address (LanguageAction language.name)) language
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
