port module Ports exposing (pause, play, playAttackSound, talk)


port outgoing : String -> Cmd msg


play : Cmd msg
play =
    outgoing "play"


pause : Cmd msg
pause =
    outgoing "pause"


talk : Cmd msg
talk =
    outgoing "talk"


playAttackSound : Cmd msg
playAttackSound =
    outgoing "talk"
