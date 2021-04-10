port module Ports exposing
    ( dhruv
    , dhruvPost
    , fanboy
    , incoherentBlacksmith
    , kelchKilled
    , kelchTaunt
    , next
    , nickKilled
    , nickTaunt
    , pause
    , play
    , scottGiveKey
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


dhruvPost : Cmd msg
dhruvPost =
    outgoing "dhruvPost"


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


scottGiveKey : Cmd msg
scottGiveKey =
    outgoing "scottGiveKey"


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
