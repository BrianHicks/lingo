module Main (..) where

import Effects
import Html exposing (Html)
import StartApp
import Task
import Lingo


app : StartApp.App Lingo.Model
app =
  StartApp.start
    { init = ( Lingo.init, Effects.none )
    , update = Lingo.update
    , view = Lingo.view
    , inits = []
    , inputs = []
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
