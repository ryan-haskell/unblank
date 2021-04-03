module Pages.Home_ exposing (Model, Msg, page)

import Browser.Events
import Color exposing (Color)
import Elm2D
import Elm2D.Spritesheet exposing (Spritesheet)
import Html
import Html.Attributes as Attr
import Json.Decode as Json
import Page
import Request exposing (Request)
import Set exposing (Set)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { man : Maybe Spritesheet
    , player : Player
    , keys : Set Key
    , counter : Int
    }


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


init : ( Model, Cmd Msg )
init =
    ( { man = Nothing
      , player =
            { x = 400 - 32
            , y = 300 - 32
            , direction = Right
            , animation = Idle
            }
      , counter = 0
      , keys = Set.empty
      }
    , Elm2D.Spritesheet.load
        { tileSize = 20
        , file = "/images/human_regular_hair.png"
        , onLoad = SpritesheetLoaded
        }
    )



-- UPDATE


type Msg
    = SpritesheetLoaded (Maybe Spritesheet)
    | KeyDown Key
    | KeyUp Key
    | Frame Float
    | AnimationTick


type alias Key =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpritesheetLoaded maybeSpritesheet ->
            ( { model | man = maybeSpritesheet }, Cmd.none )

        KeyDown key ->
            ( { model | keys = Set.insert key model.keys }, Cmd.none )

        KeyUp key ->
            ( { model | keys = Set.remove key model.keys }, Cmd.none )

        Frame dt ->
            ( { model | player = updatePlayer dt model.keys model.player }, Cmd.none )

        AnimationTick ->
            ( { model | counter = model.counter + 1 }, Cmd.none )


updatePlayer : Float -> Set Key -> Player -> Player
updatePlayer dt keys player =
    let
        speed =
            dt * 0.3

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

        animation : Animation
        animation =
            if ( dx, dy ) == ( 0, 0 ) then
                Idle

            else
                Running
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
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoderFor KeyDown)
        , Browser.Events.onKeyUp (keyDecoderFor KeyUp)
        , Browser.Events.onAnimationFrameDelta Frame
        , Time.every 200 (\_ -> AnimationTick)
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


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        case model.man of
            Just man ->
                let
                    row =
                        case model.player.direction of
                            Left ->
                                0

                            Right ->
                                2
                in
                [ Elm2D.view
                    { background = colors.offwhite
                    , size = ( 800, 600 )
                    }
                    [ Elm2D.sprite
                        { sprite =
                            case model.player.animation of
                                Idle ->
                                    Elm2D.Spritesheet.select man ( 0, row )

                                Running ->
                                    Elm2D.Spritesheet.frame (modBy 2 model.counter)
                                        (Elm2D.Spritesheet.animation man [ ( 1, row ), ( 2, row ) ])
                        , size = ( 60, 60 )
                        , position = ( model.player.x, model.player.y )
                        }
                    ]
                ]

            Nothing ->
                [ Html.text "Image not loaded" ]
    }
