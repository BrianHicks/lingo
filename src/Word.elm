module Word (..) where

import String


-- MODEL


type Level
  = Unknown
  | New
  | SeenBefore
  | AlmostKnown
  | WellKnown
  | Ignored


defaultLevel : Level
defaultLevel = Unknown


type alias Model =
  { word : String
  , level : Level
  , meaning : Maybe String
  , example : Maybe String
  , exported : Bool
  }


init : String -> Model
init word =
  { word = word
  , level = defaultLevel
  , meaning = Nothing
  , example = Nothing
  , exported = False
  }



-- UTILITY


levelClass : Level -> String
levelClass =
  toString >> String.toLower
