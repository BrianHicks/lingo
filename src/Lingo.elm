module Lingo (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
import Language
import Languages
import Routing
import Source


-- MODEL


type alias Model =
  { location : Routing.Model
  , languages : Languages.Model
  }


init : Model
init =
  { location = Routing.init
  , languages = Languages.init
  }



-- ACTION


type Action
  = RoutingAction Routing.Action
  | LanguagesAction Languages.Action



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



-- VIEW


languagesPath =
  "languages"


navItem : String -> String -> Html
navItem caption location =
  Html.li
    []
    [ Html.a
        [ Attributes.href location ]
        [ Html.text caption ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    content =
      case model.location.below of
        [] ->
          Html.p [] [ Html.text "Hello! Navigate!" ]

        languagesPath :: _ ->
          Languages.route (Routing.pop model.location) (Signal.forwardTo address LanguagesAction) model.languages
  in
    Html.div
      []
      [ model |> toString |> Html.text
      , Html.nav
          []
          [ Html.h2 [] [ Html.text "Navigation" ]
          , Html.ul
              []
              (List.map (\( caption, url ) -> navItem caption url) [ ( "Languages", (Routing.below languagesPath model.location) ) ])
          ]
      , Html.div
          []
          [ Html.h2 [] [ Html.text "Content" ]
          , content
          ]
      ]
