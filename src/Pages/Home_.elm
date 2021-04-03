module Pages.Home_ exposing (Model, Msg, page)

import Browser.Events
import Color exposing (Color)
import Elm2D
import Elm2D.Spritesheet exposing (Sprite, Spritesheet)
import Json.Decode as Json
import Page
import Request exposing (Request)
import Set exposing (Set)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { spriteMan : Maybe Spritesheet
    , spriteItems : Maybe Spritesheet
    , player : Player
    , keys : Set Key
    , time : Int
    }


type Image
    = Mans
    | Items


type alias Player =
    { x : Float
    , y : Float
    , direction : Direction
    , animation : Animation
    }


type Animation
    = Idle
    | Running


type Direction
    = Left
    | Right


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { spriteMan = Nothing
      , spriteItems = Nothing
      , player =
            { x = 400 - 32
            , y = 300 - 32
            , direction = Right
            , animation = Idle
            }
      , time = shared.initialTime
      , keys = Set.empty
      }
    , Cmd.batch
        [ Elm2D.Spritesheet.load
            { tileSize = 20
            , file = "/images/human_regular_hair.png"
            , onLoad = SpritesheetLoaded Mans
            }
        , Elm2D.Spritesheet.load
            { tileSize = 16
            , file = "/images/spritesheet_16x16.png"
            , onLoad = SpritesheetLoaded Items
            }
        ]
    )



-- UPDATE


type Msg
    = SpritesheetLoaded Image (Maybe Spritesheet)
    | KeyDown Key
    | KeyUp Key
    | Frame Time.Posix


type alias Key =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpritesheetLoaded Mans maybeSpritesheet ->
            ( { model | spriteMan = maybeSpritesheet }, Cmd.none )

        SpritesheetLoaded Items maybeSpritesheet ->
            ( { model | spriteItems = maybeSpritesheet }, Cmd.none )

        KeyDown key ->
            ( { model | keys = Set.insert key model.keys }, Cmd.none )

        KeyUp key ->
            ( { model | keys = Set.remove key model.keys }, Cmd.none )

        Frame posix ->
            let
                time =
                    Time.posixToMillis posix

                dt =
                    time - model.time
            in
            ( { model
                | player = updatePlayer (toFloat dt) model.keys model.player
                , time = time
              }
            , Cmd.none
            )


updatePlayer : Float -> Set Key -> Player -> Player
updatePlayer dt keys player =
    let
        animation : Animation
        animation =
            if ( dx, dy ) == ( 0, 0 ) then
                Idle

            else
                Running

        speed =
            dt * 0.25

        move : Key -> ( Float, Float ) -> ( Float, Float )
        move key ( x, y ) =
            case key of
                "left" ->
                    ( x - 1, y )

                "right" ->
                    ( x + 1, y )

                "up" ->
                    ( x, y - 1 )

                "down" ->
                    ( x, y + 1 )

                _ ->
                    ( x, y )

        ( dx, dy ) =
            Set.foldl move ( 0, 0 ) keys
                |> normalize
                |> Tuple.mapBoth ((*) speed) ((*) speed)

        normalize : ( Float, Float ) -> ( Float, Float )
        normalize ( x, y ) =
            if x == 0 || y == 0 then
                ( x, y )

            else
                ( x / sqrt 2, y / sqrt 2 )
    in
    { player
        | x = player.x + dx
        , y = player.y + dy
        , direction =
            if Set.member "left" keys then
                Left

            else if Set.member "right" keys then
                Right

            else
                player.direction
        , animation = animation
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoderFor KeyDown)
        , Browser.Events.onKeyUp (keyDecoderFor KeyUp)
        , Browser.Events.onAnimationFrame Frame
        ]


keyDecoderFor : (Key -> Msg) -> Json.Decoder Msg
keyDecoderFor toMsg =
    let
        handleKeyCode : String -> Json.Decoder Msg
        handleKeyCode code =
            case code of
                "KeyA" ->
                    Json.succeed (toMsg "left")

                "KeyD" ->
                    Json.succeed (toMsg "right")

                "KeyW" ->
                    Json.succeed (toMsg "up")

                "KeyS" ->
                    Json.succeed (toMsg "down")

                _ ->
                    Json.fail "Ignored keypress"
    in
    Json.field "code" Json.string
        |> Json.andThen handleKeyCode



-- VIEW


colors :
    { offwhite : Color
    }
colors =
    { offwhite = Color.rgb255 130 190 150
    }


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Unblank"
    , body =
        [ Elm2D.viewScaled
            { background = colors.offwhite
            , size = ( 800, 450 )
            , window = ( shared.window.width, shared.window.height )
            }
            (case ( model.spriteMan, model.spriteItems ) of
                ( Just man, Just items ) ->
                    [ Elm2D.sprite
                        { sprite = Elm2D.Spritesheet.select items ( 15, 20 )
                        , size = ( 32, 32 )
                        , position =
                            ( 300
                            , 100 - (4 * sin (toFloat model.time / 600) * sin (toFloat model.time / 600))
                            )
                        }
                    , Elm2D.sprite
                        { sprite = viewPlayer man model
                        , size = ( 80, 80 )
                        , position = ( model.player.x, model.player.y )
                        }
                    ]

                _ ->
                    []
            )
        ]
    }


viewPlayer : Spritesheet -> { model | time : Int, player : Player } -> Sprite
viewPlayer spritesheet model =
    let
        row =
            case model.player.direction of
                Left ->
                    0

                Right ->
                    1
    in
    case model.player.animation of
        Idle ->
            Elm2D.Spritesheet.select spritesheet ( 0, row )

        Running ->
            Elm2D.Spritesheet.frame (modBy 2 (model.time // 150))
                (Elm2D.Spritesheet.animation spritesheet [ ( 1, row ), ( 2, row ) ])
