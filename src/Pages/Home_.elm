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
    { spritesheet : Maybe Spritesheet
    , player : Player
    , keys : Set Key
    , time : Int
    , items : List Item
    }


type alias Player =
    { x : Float
    , y : Float
    , direction : Direction
    , animation : Animation
    , items : List Item
    }


type Animation
    = Idle
    | Running


type Direction
    = Left
    | Right


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { spritesheet = Nothing
      , player =
            { x = 400 - 32
            , y = 300 - 32
            , direction = Right
            , animation = Idle
            , items = []
            }
      , time = shared.initialTime
      , keys = Set.empty
      , items =
            [ Sword ( 300, 100 )
            ]
      }
    , Elm2D.Spritesheet.load
        { tileSize = 20
        , file = "/images/sprites.png"
        , onLoad = SpritesheetLoaded
        }
    )



-- UPDATE


type Msg
    = SpritesheetLoaded (Maybe Spritesheet)
    | KeyDown Key
    | KeyUp Key
    | Frame Time.Posix


type alias Key =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpritesheetLoaded spritesheet ->
            ( { model | spritesheet = spritesheet }, Cmd.none )

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

                player =
                    updatePlayer (toFloat dt) model.keys model.player

                ( pickedUpItems, remainingItems ) =
                    handleItemPickup player model.items
            in
            ( { model
                | player = { player | items = player.items ++ pickedUpItems }
                , time = time
                , items = remainingItems
              }
            , Cmd.none
            )


sizes :
    { player : Float
    , item : Float
    }
sizes =
    { player = 60
    , item = 32
    }


handleItemPickup : Player -> List Item -> ( List Item, List Item )
handleItemPickup player items =
    List.partition
        (\(Sword ( x, y )) ->
            doSquaresCollide
                { size = sizes.player, x = player.x, y = player.y }
                { size = sizes.item, x = x, y = y }
        )
        items


doSquaresCollide :
    { x : Float, y : Float, size : Float }
    -> { x : Float, y : Float, size : Float }
    -> Bool
doSquaresCollide a b =
    let
        onAxis fn =
            (fn a > fn b - a.size) && (fn a < fn b + b.size)
    in
    onAxis .x && onAxis .y


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


type Item
    = Sword ( Float, Float )


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        sprites =
            { sword = ( 0, 2 )
            }

        viewItem spritesheet (Sword ( x, y )) =
            Elm2D.sprite
                { sprite = Elm2D.Spritesheet.select spritesheet sprites.sword
                , size = ( sizes.item, sizes.item )
                , position =
                    ( x
                    , y - (4 * sin (0.004 * toFloat model.time))
                    )
                }
    in
    { title = "Unblank"
    , body =
        [ Elm2D.viewScaled
            { background = colors.offwhite
            , size = ( 800, 450 )
            , window = ( shared.window.width, shared.window.height )
            }
            (case model.spritesheet of
                Just spritesheet ->
                    List.concat
                        [ List.map (viewItem spritesheet) model.items
                        , [ Elm2D.sprite
                                { sprite = viewPlayer spritesheet model
                                , size = ( sizes.player, sizes.player )
                                , position = ( model.player.x, model.player.y )
                                }
                          ]
                        ]

                Nothing ->
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
            case model.player.items of
                [] ->
                    Elm2D.Spritesheet.select spritesheet ( 0, row )

                _ ->
                    Elm2D.Spritesheet.select spritesheet ( 3, row )

        Running ->
            Elm2D.Spritesheet.frame (modBy 2 (model.time // 150))
                (Elm2D.Spritesheet.animation spritesheet [ ( 1, row ), ( 2, row ) ])
