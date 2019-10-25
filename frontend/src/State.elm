module State exposing (..)



type alias State =
    { url : String
    }


toString : State -> String
toString state =
    state.url


default : State
default =
    { url = ""
    }
