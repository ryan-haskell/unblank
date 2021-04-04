module Pages.Home_ exposing (Model, Msg, page)

import Bitmap exposing (Bitmap)
import Browser.Events
import Color exposing (Color)
import Dict exposing (Dict)
import Elm2D
import Elm2D.Spritesheet exposing (Sprite, Spritesheet)
import Http
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
    , mouse : MouseState
    , keys : Set Key
    , time : Int
    , items : List Item
    , world : Dict ( Int, Int ) Tile
    }


type Tile
    = Tree
    | Water


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
    | Attacking Int


type Direction
    = Left
    | Right


camera :
    { width : Float
    , height : Float
    }
camera =
    { width = 800
    , height = 450
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { spritesheet = Nothing
      , player =
            { x = 124 * sizes.tile
            , y = 55 * sizes.tile
            , direction = Left
            , animation = Idle
            , items = []
            }
      , time = shared.initialTime
      , keys = Set.empty
      , mouse = Up
      , world = Dict.empty
      , items =
            [ Sword ( 120 * sizes.tile, 60 * sizes.tile )
            ]
      }
    , Cmd.batch
        [ Elm2D.Spritesheet.load
            { tileSize = 16
            , file = "/images/sprites.png"
            , onLoad = SpritesheetLoaded
            }
        , Http.get
            { url = "/images/world.bmp"
            , expect = Http.expectBytes GotWorld Bitmap.decoder
            }
        ]
    )



-- UPDATE


type Msg
    = SpritesheetLoaded (Maybe Spritesheet)
    | GotWorld (Result Http.Error Bitmap)
    | KeyDown Key
    | KeyUp Key
    | Frame Time.Posix
    | Mouse MouseState


type MouseState
    = Down
    | Up


type alias Key =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWorld (Ok bitmap) ->
            let
                world =
                    Bitmap.toDict bitmap
                        |> Dict.toList
                        |> List.filterMap
                            (\( k, v ) ->
                                (case v of
                                    ( 21, 111, 48 ) ->
                                        Just Tree

                                    ( 153, 217, 234 ) ->
                                        Just Water

                                    _ ->
                                        Nothing
                                )
                                    |> Maybe.map (Tuple.pair k)
                            )
                        |> Dict.fromList
            in
            ( { model | world = world }, Cmd.none )

        GotWorld _ ->
            ( model, Cmd.none )

        Mouse state ->
            ( { model | mouse = state }, Cmd.none )

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
                    updatePlayer (toFloat dt) model

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
    , tile : Float
    }
sizes =
    { player = 64
    , item = 32
    , tile = 32
    }


handleItemPickup : Player -> List Item -> ( List Item, List Item )
handleItemPickup player items =
    List.partition
        (\(Sword ( x, y )) ->
            doRectanglesCollide
                { width = sizes.player, height = sizes.player, x = player.x, y = player.y }
                { width = sizes.item, height = sizes.item, x = x, y = y }
        )
        items


doRectanglesCollide :
    { x : Float, y : Float, width : Float, height : Float }
    -> { x : Float, y : Float, width : Float, height : Float }
    -> Bool
doRectanglesCollide a b =
    let
        onAxis fn size =
            (fn a > fn b - size a) && (fn a < fn b + size b)
    in
    onAxis .x .width && onAxis .y .height


keys : { left : Key, right : Key, up : Key, down : Key }
keys =
    { left = "left"
    , right = "right"
    , up = "up"
    , down = "down"
    }


deltaDict : Dict Key ( Float, Float )
deltaDict =
    Dict.fromList
        [ ( keys.left, ( -1, 0 ) )
        , ( keys.right, ( 1, 0 ) )
        , ( keys.up, ( 0, -1 ) )
        , ( keys.down, ( 0, 1 ) )
        ]


move : Key -> ( Float, Float ) -> ( Float, Float )
move key ( x, y ) =
    Dict.get key deltaDict
        |> Maybe.withDefault ( 0, 0 )
        |> Tuple.mapBoth ((+) x) ((+) y)


normalize : ( Float, Float ) -> ( Float, Float )
normalize ( x, y ) =
    if x == 0 || y == 0 then
        ( x, y )

    else
        ( x / sqrt 2, y / sqrt 2 )


updatePlayer : Float -> Model -> Player
updatePlayer dt ({ mouse, player } as model) =
    let
        speed =
            dt * 0.25

        ( dx, dy ) =
            Set.foldl move ( 0, 0 ) model.keys
                |> normalize
                |> Tuple.mapBoth ((*) speed) ((*) speed)

        isAttacking =
            case mouse of
                Down ->
                    hasSword player

                Up ->
                    False

        animation : Animation
        animation =
            if isAttacking then
                Attacking 1

            else if ( dx, dy ) == ( 0, 0 ) then
                Idle

            else
                Running

        full =
            sizes.player

        quarter =
            sizes.player / 4

        hasCollision ( x, y ) =
            List.any collideWithTerrain
                [ ( x + quarter, y + full - quarter )
                , ( x + full - quarter, y + full - quarter )
                , ( x + quarter, y + full - 4 )
                , ( x + full - quarter, y + full - 4 )
                ]

        collideWithTerrain ( x, y ) =
            Dict.member
                ( floor (x / sizes.tile)
                , floor (y / sizes.tile)
                )
                model.world

        ( newX, newY ) =
            List.filter (hasCollision >> not)
                [ ( player.x + dx, player.y + dy )
                , if dx /= 0 then
                    ( player.x + dx, player.y )

                  else
                    ( player.x, player.y + (dy / abs dy) )
                , if dy /= 0 then
                    ( player.x, player.y + dy )

                  else
                    ( player.x + (dx / abs dx), player.y )
                ]
                |> List.head
                |> Maybe.withDefault ( player.x, player.y )
    in
    { player
        | x = newX
        , y = newY
        , direction =
            if Set.member keys.left model.keys then
                Left

            else if Set.member keys.right model.keys then
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
        , Browser.Events.onMouseDown (Json.succeed (Mouse Down))
        , Browser.Events.onMouseUp (Json.succeed (Mouse Up))
        ]


keyCodeMapping : Dict String Key
keyCodeMapping =
    Dict.fromList
        [ ( "KeyA", keys.left )
        , ( "KeyD", keys.right )
        , ( "KeyW", keys.up )
        , ( "KeyS", keys.down )
        ]


keyDecoderFor : (Key -> Msg) -> Json.Decoder Msg
keyDecoderFor toMsg =
    let
        handleKeyCode : String -> Json.Decoder Msg
        handleKeyCode code =
            Dict.get code keyCodeMapping
                |> Maybe.map (toMsg >> Json.succeed)
                |> Maybe.withDefault (Json.fail "Ignored key event")
    in
    Json.field "code"
        (Json.string |> Json.andThen handleKeyCode)



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
            { sword = ( 0, 8 )
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

        viewWorldTile : Spritesheet -> ( Int, Int ) -> Tile -> Elm2D.Element
        viewWorldTile spritesheet xy tile =
            let
                size =
                    ( sizes.tile, sizes.tile )

                position =
                    Tuple.mapBoth
                        (toFloat
                            >> (*) sizes.tile
                        )
                        (toFloat
                            >> (*) sizes.tile
                        )
                        xy
            in
            case tile of
                Tree ->
                    -- Elm2D.sprite
                    --     { sprite = Elm2D.Spritesheet.select spritesheet ( 1, 8 )
                    --     , size = size
                    --     , position = position
                    --     }
                    Elm2D.rectangle
                        { size = size
                        , position = position
                        , color = Color.rgb255 0 100 50
                        }

                Water ->
                    Elm2D.rectangle
                        { size = size
                        , position = position
                        , color = Color.lightBlue
                        }

        tiles : Spritesheet -> List Elm2D.Element
        tiles spritesheet =
            let
                bounds =
                    { left = floor ((model.player.x - camera.width / 2) / sizes.tile)
                    , top = floor ((model.player.y - camera.height / 2) / sizes.tile)
                    }

                right =
                    bounds.left + ceiling (camera.width / sizes.tile) + 1

                bottom =
                    bounds.top + ceiling (camera.height / sizes.tile) + 1
            in
            List.range bounds.left right
                |> List.concatMap
                    (\x ->
                        List.filterMap
                            (\y ->
                                Dict.get ( x, y ) model.world
                                    |> Maybe.map (viewWorldTile spritesheet ( x, y ))
                            )
                            (List.range bounds.top bottom)
                    )
    in
    { title = "Unblank"
    , body =
        [ Elm2D.viewFollowCamera
            { background = colors.offwhite
            , size = ( camera.width, camera.height )
            , window = ( shared.window.width, shared.window.height )
            , centeredOn = ( model.player.x + sizes.player / 2, model.player.y + sizes.player / 2 )
            }
            (case ( model.spritesheet, Dict.size model.world ) of
                ( _, 0 ) ->
                    []

                ( Nothing, _ ) ->
                    []

                ( Just spritesheet, _ ) ->
                    List.concat
                        [ tiles spritesheet
                        , List.map (viewItem spritesheet) model.items
                        , [ Elm2D.sprite
                                { sprite = viewPlayer spritesheet model
                                , size = ( sizes.player, sizes.player )
                                , position = ( model.player.x, model.player.y )
                                }
                          ]
                        ]
            )
        ]
    }


hasSword : Player -> Bool
hasSword player =
    player.items /= []


viewPlayer : Spritesheet -> { model | time : Int, player : Player } -> Sprite
viewPlayer spritesheet model =
    let
        itemOffset =
            if hasSword model.player then
                2

            else
                0

        col =
            case model.player.direction of
                Right ->
                    0 + itemOffset

                Left ->
                    1 + itemOffset
    in
    case model.player.animation of
        Idle ->
            Elm2D.Spritesheet.select spritesheet ( col, 0 )

        Running ->
            Elm2D.Spritesheet.frame (modBy 4 (model.time // 200))
                (Elm2D.Spritesheet.animation spritesheet [ ( col, 1 ), ( col, 0 ), ( col, 2 ), ( col, 0 ) ])

        Attacking frame ->
            Elm2D.Spritesheet.select spritesheet ( col, 4 )
