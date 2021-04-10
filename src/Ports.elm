port module Ports exposing
    ( dhruv
    , fanboy
    , incoherentBlacksmith
    , kelchKilled
    , kelchTaunt
    , next
    , nickKilled
    , nickTaunt
    , pause
    , play
    , scottPost
    , scottPre
    , shadyIndividual
    , snootyLady
    )


port outgoing : String -> Cmd msg


play : Cmd msg
play =
    outgoing "play"


pause : Cmd msg
pause =
    outgoing "pause"


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


nickTaunt =
    outgoing "nickTaunt"


nickKilled =
    outgoing "nickKilled"


scottPre : Cmd msg
scottPre =
    outgoing "scottPre"


scottPost : Cmd msg
scottPost =
    outgoing "scottPost"


snootyLady : Cmd msg
snootyLady =
    outgoing "snootyLady"


fanboy : Cmd msg
fanboy =
    outgoing "snootyLady"


incoherentBlacksmith : Cmd msg
incoherentBlacksmith =
    outgoing "incoherentBlacksmith"


shadyIndividual : Cmd msg
shadyIndividual =
    outgoing "shadyIndividual"
