module Pages.Home_ exposing (Model, Msg, page)

import Bitmap exposing (Bitmap)
import Browser.Events
import Color exposing (Color)
import Dict exposing (Dict)
import Elm2D
import Elm2D.Spritesheet exposing (Sprite, Spritesheet)
import Html
import Html.Attributes as Attr
import Http
import Json.Decode as Json
import Page
import Ports
import Request exposing (Request)
import Set exposing (Set)
import Shared
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { inPauseMenu : Bool
    , spritesheet : Maybe Spritesheet
    , player : Player
    , mouse : MouseState
    , keys : Set Key
    , ticks : Float
    , items : List Item
    , enemies : List Enemy
    , world : Dict ( Int, Int ) Tile
    , npcs : List Npc
    }


type Npc
    = Npc { name : String, x : Float, y : Float, direction : Direction }


type Enemy
    = Goblin Direction Animation ( Float, Float )


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


init : ( Model, Cmd Msg )
init =
    ( { inPauseMenu = True
      , spritesheet = Nothing
      , player =
            { x = 124 * sizes.tile
            , y = 55 * sizes.tile
            , direction = Left
            , animation = Idle
            , items = []
            }
      , ticks = 0
      , keys = Set.empty
      , mouse = Up
      , enemies =
            [ Goblin Right Idle ( 118 * sizes.tile, 64 * sizes.tile )
            ]
      , world = Dict.empty
      , items =
            [ Sword ( 120 * sizes.tile, 60 * sizes.tile )
            ]
      , npcs =
            [ Npc
                { name = "Dhruv"
                , x = 130 * sizes.tile
                , y = 50 * sizes.tile
                , direction = Left
                }
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
    | Frame Float
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
            let
                inPauseMenu =
                    if key == keys.menu then
                        not model.inPauseMenu

                    else
                        model.inPauseMenu

                toggleMusicCmd =
                    case ( model.inPauseMenu, inPauseMenu ) of
                        ( True, False ) ->
                            Ports.play

                        ( False, True ) ->
                            Ports.pause

                        _ ->
                            Cmd.none
            in
            ( { model | keys = Set.remove key model.keys, inPauseMenu = inPauseMenu }
            , Cmd.batch
                [ toggleMusicCmd
                , case ( nearbyNpc model, key == keys.interact ) of
                    ( Just npc, True ) ->
                        Ports.talk

                    _ ->
                        Cmd.none
                ]
            )

        Frame dt ->
            let
                player =
                    updatePlayer dt model

                ( pickedUpItems, remainingItems ) =
                    handleItemPickup player model.items

                enemies =
                    List.map (updateEnemy dt model) model.enemies
            in
            ( { model
                | player = { player | items = player.items ++ pickedUpItems }
                , ticks = model.ticks + dt
                , items = remainingItems
                , enemies = enemies
              }
            , Cmd.none
            )


updateEnemy : Float -> Model -> Enemy -> Enemy
updateEnemy dt ({ player } as model) (Goblin dir _ ( x, y )) =
    let
        inAttackRange =
            doSquaresCollide
                { x = model.player.x, y = model.player.y, size = sizes.player }
                { x = x, y = y, size = sizes.goblin }
    in
    if inAttackRange then
        Goblin dir Idle ( x, y )

    else
        let
            speed =
                0.12 * dt

            moveTowardPlayer :
                (Player -> Float)
                -> ({ x : Float, y : Float } -> Float)
                -> Float
            moveTowardPlayer f g =
                if floor (f player - g goblin) < 0 then
                    -1

                else if floor (f player - g goblin) > 0 then
                    1

                else
                    0

            ( dx, dy ) =
                ( moveTowardPlayer .x .x
                , moveTowardPlayer .y .y
                )
                    |> normalize
                    |> Tuple.mapBoth ((*) speed) ((*) speed)

            goblin : { x : Float, y : Float }
            goblin =
                { x = x, y = y }

            animation =
                if newX == x && newY == y then
                    Idle

                else
                    Running

            direction =
                if dx < 0 then
                    Left

                else if dx > 0 then
                    Right

                else
                    dir

            ( newX, newY ) =
                attemptMovement sizes.goblin model.world goblin ( dx, dy )
        in
        Goblin direction animation ( newX, newY )


sizes :
    { player : Float
    , npc : Float
    , item : Float
    , tile : Float
    , goblin : Float
    }
sizes =
    { player = 64
    , npc = 64
    , goblin = 64
    , item = 32
    , tile = 32
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


keys :
    { left : String
    , right : String
    , up : String
    , down : String
    , menu : String
    , interact : String
    }
keys =
    { left = "left"
    , right = "right"
    , up = "up"
    , down = "down"
    , menu = "menu"
    , interact = "interact"
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

        ( newX, newY ) =
            attemptMovement sizes.player model.world player ( dx, dy )
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


attemptMovement : Float -> Dict ( Int, Int ) v -> { a | x : Float, y : Float } -> ( Float, Float ) -> ( Float, Float )
attemptMovement size world mob ( dx, dy ) =
    let
        quarter =
            size / 4

        hasCollision ( x, y ) =
            List.any collideWithTerrain
                [ ( x + quarter, y + size - quarter )
                , ( x + size - quarter, y + size - quarter )
                , ( x + quarter, y + size - 4 )
                , ( x + size - quarter, y + size - 4 )
                ]

        collideWithTerrain ( x, y ) =
            Dict.member
                ( floor (x / sizes.tile)
                , floor (y / sizes.tile)
                )
                world
    in
    List.filter (hasCollision >> not)
        [ ( mob.x + dx, mob.y + dy )
        , if dx /= 0 then
            ( mob.x + dx, mob.y )

          else
            ( mob.x, mob.y + (dy / abs dy) )
        , if dy /= 0 then
            ( mob.x, mob.y + dy )

          else
            ( mob.x + (dx / abs dx), mob.y )
        ]
        |> List.head
        |> Maybe.withDefault ( mob.x, mob.y )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.concat
            [ [ Browser.Events.onKeyDown (keyDecoderFor KeyDown)
              , Browser.Events.onKeyUp (keyDecoderFor KeyUp)
              ]
            , if model.inPauseMenu then
                []

              else
                [ Browser.Events.onAnimationFrameDelta Frame
                , Browser.Events.onMouseDown (Json.succeed (Mouse Down))
                , Browser.Events.onMouseUp (Json.succeed (Mouse Up))
                ]
            ]


keyCodeMapping : Dict String Key
keyCodeMapping =
    Dict.fromList
        [ ( "KeyA", keys.left )
        , ( "KeyD", keys.right )
        , ( "KeyW", keys.up )
        , ( "KeyS", keys.down )
        , ( "Escape", keys.menu )
        , ( "KeyE", keys.interact )
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


type alias Square =
    { x : Float, y : Float, size : Float }


npcToSquare : Npc -> Square
npcToSquare (Npc npc) =
    { x = npc.x
    , y = npc.y
    , size = sizes.npc
    }


playerToSquare : Player -> Square
playerToSquare player =
    { x = player.x
    , y = player.y
    , size = sizes.player
    }


nearbyNpc : Model -> Maybe Npc
nearbyNpc model =
    model.npcs
        |> List.filter (\npc -> doSquaresCollide (npcToSquare npc) (playerToSquare model.player))
        |> List.head


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
                    , y - (4 * sin (0.004 * model.ticks))
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
        [ Html.div [ Attr.class "game" ]
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
                            , List.concat
                                [ List.map (viewEnemy spritesheet model) model.enemies
                                , List.map (viewNpc spritesheet) model.npcs
                                , [ ( model.player.y
                                    , Elm2D.sprite
                                        { sprite = viewPlayer spritesheet model
                                        , size = ( sizes.player, sizes.player )
                                        , position = ( model.player.x, model.player.y )
                                        }
                                    )
                                  ]
                                ]
                                |> List.sortBy Tuple.first
                                |> List.map Tuple.second
                            ]
                )
            , case nearbyNpc model of
                Just (Npc npc) ->
                    Html.div [ Attr.class "dialogue-prompt" ]
                        [ Html.text "Talk to "
                        , Html.strong [] [ Html.text npc.name ]
                        ]

                Nothing ->
                    Html.text ""
            ]
        , if model.inPauseMenu then
            Html.div [ Attr.class "overlay" ]
                [ Html.text "Press ESC to continue"
                ]

          else
            Html.text ""
        ]
    }


hasSword : Player -> Bool
hasSword player =
    player.items /= []


viewPlayer : Spritesheet -> { model | ticks : Float, player : Player } -> Sprite
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
            Elm2D.Spritesheet.frame (modBy 4 (round model.ticks // 200))
                (Elm2D.Spritesheet.animation spritesheet [ ( col, 1 ), ( col, 0 ), ( col, 2 ), ( col, 0 ) ])

        Attacking _ ->
            Elm2D.Spritesheet.select spritesheet ( col, 4 )


viewEnemy : Spritesheet -> Model -> Enemy -> ( Float, Elm2D.Element )
viewEnemy spritesheet model (Goblin dir animation position) =
    let
        col =
            case dir of
                Left ->
                    1

                Right ->
                    0
    in
    ( Tuple.second position
    , Elm2D.sprite
        { size = ( sizes.goblin, sizes.goblin )
        , position = position
        , sprite =
            case animation of
                Idle ->
                    Elm2D.Spritesheet.select spritesheet ( col, 10 )

                Running ->
                    Elm2D.Spritesheet.frame (modBy 3 (round model.ticks // 100))
                        (Elm2D.Spritesheet.animation spritesheet [ ( col, 10 ), ( col, 11 ), ( col, 12 ) ])

                Attacking frame ->
                    Elm2D.Spritesheet.select spritesheet ( col, 10 )
        }
    )


viewNpc : Spritesheet -> Npc -> ( Float, Elm2D.Element )
viewNpc spritesheet (Npc npc) =
    let
        col =
            case npc.direction of
                Right ->
                    3

                Left ->
                    4
    in
    ( npc.y
    , Elm2D.sprite
        { size = ( sizes.npc, sizes.npc )
        , position = ( npc.x, npc.y )
        , sprite = Elm2D.Spritesheet.select spritesheet ( col, 10 )
        }
    )
