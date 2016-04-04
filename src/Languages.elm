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
      { title = "Potencial de acción"
      , text = "Un potencial de acción, también llamado impulso eléctrico, es una onda de descarga eléctrica que viaja a lo largo de la membrana celular modificando su distribución de carga eléctrica. Los potenciales de acción se utilizan en el cuerpo para llevar información entre unos tejidos y otros, lo que hace que sean una característica microscópica esencial para la vida. Pueden generarse por diversos tipos de células corporales, pero las más activas en su uso son las células del sistema nervioso para enviar mensajes entre células nerviosas (sinapsis) o desde células nerviosas a otros tejidos corporales, como el músculo o las glándulas.\n\nMuchas plantas también generan potenciales de acción que viajan a través del floema para coordinar su actividad. La principal diferencia entre los potenciales de acción de animales y plantas es que las plantas utilizan flujos de potasio y calcio mientras que los animales utilizan potasio y sodio.\n\nLos potenciales de acción son la vía fundamental de transmisión de códigos neurales. Sus propiedades pueden frenar el tamaño de cuerpos en desarrollo y permitir el control y coordinación centralizados de órganos y tejidos."
      , source = Just "https://es.wikipedia.org/wiki/Potencial_de_acci%C3%B3n"
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
  case path.below of
    [] ->
      view path address model

    name :: _ ->
      case byName model name of
        Nothing ->
          Routing.notFound

        Just language ->
          Language.route (Routing.pop path) (Signal.forwardTo address (LanguageAction name)) language



-- VIEW


nav : Routing.Model -> Signal.Address Action -> Language.Model -> Html
nav path address language =
  Html.li
    []
    [ Html.a
        [ Attributes.href (path |> Routing.below language.name |> Routing.serialize) ]
        [ Html.text language.name ]
    ]


view : Routing.Model -> Signal.Address Action -> Model -> Html
view path address model =
  Html.ul [] (List.map (nav path address) model)



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
