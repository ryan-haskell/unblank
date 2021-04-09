port module Ports exposing (dhruv, kelchKilled, kelchTaunt, next, pause, play, talk)


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


dhruv : Cmd msg
dhruv =
    outgoing "dhruv"


next : Cmd msg
next =
    outgoing "next"


kelchTaunt =
    outgoing "kelchTaunt"


kelchKilled =
    outgoing "kelchKilled"
