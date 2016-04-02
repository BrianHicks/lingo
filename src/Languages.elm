module Languages (..) where

import Dict exposing (Dict)
import Effects exposing (Effects)
import Html exposing (Html)
import Html.Attributes as Attributes
import Routing
import Language


-- MODEL


type alias Model =
  List Language.Model


init : Model
init =
  let
    sampleText =
      { title = "This is a 'Test'"
      , text = "Lorem ipsum dolor sit amet pro consecorum del taco."
      , source = Just "https://blah.com/whatever"
      }

    spanish =
      Language.init "Spanish" |> Language.addSource sampleText
  in
    [ spanish ]



-- ACTION


type Action
  = AddLanguage String
  | LanguageAction String Language.Action



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    AddLanguage name ->
      ( (Language.init name) :: model, Effects.none )

    LanguageAction name action ->
      case byName model name of
        Nothing ->
          ( model, Effects.none )

        Just target ->
          let
            ( language, fx ) =
              Language.update action target
          in
            ( model |> updateIn language
            , Effects.map (LanguageAction name) fx
            )



-- ROUTER


route : Routing.Model -> Signal.Address Action -> Model -> Html
route path address model =
  case path of
    [] ->
      view address model

    name :: rest ->
      case byName model name of
        Nothing ->
          Routing.notFound

        Just language ->
          Language.route rest (Signal.forwardTo address (LanguageAction name)) language



-- VIEW


languageView : Signal.Address Action -> Language.Model -> Html
languageView address language =
  Html.li
    []
    [ Html.a
        [ Attributes.href "TODO" ]
        [ Html.text language.name ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  Html.ul [] (List.map (languageView address) model)



-- UTILITY


byName : Model -> String -> Maybe Language.Model
byName model language =
  model
    |> List.filter (\candidate -> candidate.name == language)
    |> List.head


updateIn : Language.Model -> Model -> Model
updateIn language model =
  List.map
    (\candidate ->
      if candidate.name == language.name then
        language
      else
        candidate
    )
    model
