port module Ports exposing (pause, play)


port outgoing : String -> Cmd msg


play =
    outgoing "play"


pause =
    outgoing "pause"
