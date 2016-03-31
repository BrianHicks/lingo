module Word (..) where

-- MODEL


type Level
  = Unknown
  | New
  | SeenBefore
  | AlmostKnown
  | WellKnown
  | Ignored


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
  , level = Unknown
  , meaning = Nothing
  , example = Nothing
  , exported = False
  }
