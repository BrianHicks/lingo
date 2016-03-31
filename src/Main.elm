module Main (..) where

import Effects
import History
import Html exposing (Html)
import Lingo
import Routing
import StartApp
import Task


app : StartApp.App Lingo.Model
app =
  StartApp.start
    { init = ( Lingo.init, Effects.none )
    , update = Lingo.update
    , view = Lingo.view
    , inits = [ hash ]
    , inputs = []
    }


main : Signal Html
main =
  app.html


hash : Signal Lingo.Action
hash =
  Signal.map (Routing.PathChange >> Lingo.RoutingAction) History.hash


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
