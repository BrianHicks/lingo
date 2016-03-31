module Languages (..) where

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
  [ (Language.init "Spanish") ]



-- ACTION


type Action
  = AddLanguage String



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    AddLanguage name ->
      ( (Language.init name) :: model, Effects.none )



-- VIEW


languageView : Signal.Address Action -> Language.Model -> Html
languageView address language =
  Html.li
    []
    [ Html.a
        [ Attributes.href (Routing.toPath (Routing.Language language.name)) ]
        [ Html.text (Routing.toName (Routing.Language language.name)) ]
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


updateIn : Model -> Language.Model -> Model
updateIn model language =
  List.map
    (\candidate ->
      if candidate.name == language.name then
        language
      else
        candidate
    )
    model
