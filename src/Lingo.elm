module Lingo (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Shorthand as H
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
  H.li_ [ H.a_ location caption ]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    content =
      case model.location.below of
        [] ->
          H.p_ [ Html.text "Hello! Navigate!" ]

        languagesPath :: _ ->
          Languages.route (Routing.pop model.location) (Signal.forwardTo address LanguagesAction) model.languages
  in
    H.div'
      { class = "container" }
      [ Html.node "link" [ Attributes.href "../style.css", Attributes.rel "stylesheet" ] []
      , H.nav'
          { class = "nav" }
          [ H.h1_ "Lingo"
          , H.ul_
              [ H.li_
                  [ Html.h2
                      []
                      [ H.a_ (model.location |> Routing.below languagesPath |> Routing.serialize) "Languages" ]
                  , H.ul_
                      (List.map (Languages.nav (model.location |> Routing.below languagesPath) (Signal.forwardTo address LanguagesAction)) model.languages)
                  ]
              ]
          ]
      , H.div'
          { class = "content" }
          [ content ]
      ]
