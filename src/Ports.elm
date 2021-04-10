port module Ports exposing
    ( bossIntro
    , bossKilled
    , dhruv
    , dhruvPost
    , fanboy
    , incoherentBlacksmith
    , kelchHit
    , kelchKilled
    , kelchTaunt
    , next
    , nickHit
    , nickKilled
    , nickTaunt
    , pause
    , play
    , scottGiveKey
    , scottPost
    , scottPre
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


kelchHit =
    outgoing "kelchHit"


kelchKilled =
    outgoing "kelchKilled"


nickTaunt =
    outgoing "nickTaunt"


nickHit =
    outgoing "nickHit"


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
    outgoing "lady"


fanboy : Cmd msg
fanboy =
    outgoing "boy"


incoherentBlacksmith : Cmd msg
incoherentBlacksmith =
    outgoing "incoherentBlacksmith"


bossIntro : Cmd msg
bossIntro =
    outgoing "bossIntro"


bossKilled : Cmd msg
bossKilled =
    outgoing "bossKilled"
