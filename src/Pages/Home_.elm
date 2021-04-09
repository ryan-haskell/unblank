module Pages.Home_ exposing (Model, Msg, page)

import Bitmap exposing (Bitmap)
import Browser.Events
import Color
import Dict exposing (Dict)
import Elm2D
import Elm2D.Spritesheet exposing (Sprite, Spritesheet, animation)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode as Json
import Page
import Ports
import Request exposing (Request)
import Set exposing (Set)
import Shared
import Task
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
    { phase : Phase
    , menu : Menu
    , spritesheet : Maybe Spritesheet
    , player : Player
    , mouse : MouseState
    , keys : Set Key
    , ticks : Float
    , items : List Item
    , enemies : List Enemy
    , world : World
    , npcs : List Npc
    , visited : Set ( Int, Int )
    }


type alias World =
    Dict ( Int, Int ) Tile


type Npc
    = Npc
        { name : String
        , kind : NpcKind
        , x : Float
        , y : Float
        , direction : Direction
        , onSpeak : Msg
        }


type NpcKind
    = Orc


type alias Enemy =
    { id : Int
    , kind : EnemyKind
    , direction : Direction
    , animation : Animation
    , x : Float
    , y : Float
    , health : Int
    , attackTimer : Float
    , isAttacking : Bool
    , timeSpentStuck : Float
    }


isPassive : Enemy -> Bool
isPassive enemy =
    case enemy.kind of
        Pig ->
            True

        RagingPig ->
            False

        Sandit ->
            False

        Kelch ->
            True


type EnemyKind
    = Pig
    | RagingPig
    | Sandit
    | Kelch


damagePerAttack : Enemy -> number
damagePerAttack enemy =
    case enemy.kind of
        Pig ->
            0

        Kelch ->
            0

        RagingPig ->
            1

        Sandit ->
            2


type Tile
    = Tree
    | Water
    | Bridge
    | UnderwaterBridge
    | Sand
    | DarkGrass
    | Gravel
    | ColdLava
    | Unknown ( Int, Int, Int )


walkable : Phase -> List Tile
walkable phase =
    let
        tiles =
            [ Sand
            , DarkGrass
            , ColdLava
            , Gravel
            ]
    in
    if (phases phase).canUseBridge then
        Bridge :: tiles

    else
        tiles


type alias Player =
    { x : Float
    , y : Float
    , direction : Direction
    , animation : Animation
    , attackTimer : Float
    , isAttacking : Bool
    , isBlocking : Bool
    , fireball : Maybe Projectile
    , health : Int
    , maxHealth : Int
    , gold : Int
    , hasSword : Bool
    , hasShield : Bool
    , hasDash : Bool
    , dashElapsed : Float
    }


type alias Projectile =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , duration : Float
    , target : Maybe Enemy
    }


type Animation
    = Idle
    | Running
    | Attacking Float Int
    | Blocking


type Direction
    = Left
    | Right


camera :
    { width : Float
    , height : Float
    }
camera =
    { width = 1200
    , height = 675
    }


type Menu
    = MainMenu
    | PauseMenu
    | ShopMenu
    | None


type Phase
    = LearningToMove
    | PickingUpSword Float
    | LearningToAttack Float
    | FightingFirstEnemies Float
    | PickingUpTreasure Float
    | LearningToInteract Float
    | LearningToBlock Float
    | ReadyToExplore Float


init : ( Model, Cmd Msg )
init =
    ( { phase = LearningToMove
      , menu = MainMenu
      , spritesheet = Nothing
      , player = spawnPlayer ( 0, 0 )
      , ticks = 0
      , keys = Set.empty
      , mouse = { leftClick = False, rightClick = False }
      , enemies = []
      , world = Dict.empty
      , items = []
      , npcs = []
      , visited = Set.empty
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
    | StartGame
    | KeyDown Key
    | KeyUp Key
    | Frame Float
    | MouseUp MouseButton
    | MouseDown MouseButton
    | OpenShopMenu
    | SayThings
    | CloseShopMenu
    | BoughtShield


type MouseButton
    = LeftClick
    | RightClick


type alias MouseState =
    { leftClick : Bool
    , rightClick : Bool
    }


type alias Key =
    String


colors =
    { bridge = ( 185, 122, 87 )
    , underwaterBridge = ( 112, 146, 190 )
    , sand = ( 239, 228, 176 )
    , gravel = ( 195, 195, 195 )
    , darkGrass = ( 106, 159, 124 )
    , tree = ( 21, 111, 48 )
    , grass = ( 130, 190, 150 )
    , chest = ( 0, 162, 232 )
    , coldLava = ( 136, 11, 17 )
    , water = ( 140, 211, 232 )
    , sandit = ( 128, 64, 0 )
    , ragingPig = ( 237, 28, 36 )
    , kelch = ( 64, 0, 128 )
    }


colorsToTile : Dict ( Int, Int, Int ) Tile
colorsToTile =
    Dict.fromList
        [ ( colors.bridge, Bridge )
        , ( colors.underwaterBridge, UnderwaterBridge )
        , ( colors.sand, Sand )
        , ( colors.gravel, Gravel )
        , ( colors.coldLava, ColdLava )
        , ( colors.darkGrass, DarkGrass )
        , ( colors.tree, Tree )
        , ( colors.water, Water )
        ]


spawnPlayer : ( Float, Float ) -> Player
spawnPlayer ( x, y ) =
    { x = x * sizes.tile
    , y = y * sizes.tile
    , direction = Left
    , animation = Idle
    , attackTimer = 0
    , isAttacking = False
    , isBlocking = False
    , fireball = Nothing
    , gold = 0
    , health = 25
    , maxHealth = 25
    , hasSword = False
    , hasShield = False
    , hasDash = False
    , dashElapsed = 0
    }


dash =
    { cooldown = 1000
    , duration = 300
    }


hackPlayer : Player
hackPlayer =
    { x = 25 * sizes.tile
    , y = 75 * sizes.tile
    , direction = Left
    , animation = Idle
    , attackTimer = 0
    , isAttacking = False
    , isBlocking = False
    , fireball = Nothing
    , gold = 0
    , health = 25
    , maxHealth = 25
    , hasSword = True
    , hasShield = True
    , hasDash = True
    , dashElapsed = 0
    }


spawnEnemy : Int -> EnemyKind -> ( Float, Float ) -> Enemy
spawnEnemy id kind ( x, y ) =
    case kind of
        Kelch ->
            Enemy id kind Right Idle x y 50 0 False 0

        _ ->
            Enemy id kind Right Idle x y 3 0 False 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { mouse } =
            model

        player_ =
            model.player
    in
    case msg of
        GotWorld (Ok bitmap) ->
            let
                loop :
                    ( Int, Int )
                    -> ( Int, Int, Int )
                    -> { player : Player, npcs : List Npc, enemies : List Enemy, world : Dict ( Int, Int ) Tile, items : List Item }
                    -> { player : Player, npcs : List Npc, enemies : List Enemy, world : Dict ( Int, Int ) Tile, items : List Item }
                loop ( x, y ) color data =
                    let
                        coordinate =
                            ( toFloat x, toFloat y )

                        place tile =
                            Dict.insert ( x, y ) tile data.world

                        addNpc name kind action =
                            Npc
                                { name = name
                                , kind = kind
                                , x = Tuple.first coordinate * sizes.tile
                                , y = Tuple.second coordinate * sizes.tile
                                , direction = Left
                                , onSpeak = action
                                }
                                :: data.npcs

                        chest =
                            { kind = Gem
                            , x = Tuple.first coordinate * sizes.tile
                            , y = Tuple.second coordinate * sizes.tile
                            }

                        addEnemy kind =
                            spawnEnemy (List.length data.enemies)
                                kind
                                ( Tuple.first coordinate * sizes.tile
                                , Tuple.second coordinate * sizes.tile
                                )
                                :: data.enemies
                    in
                    if color == colors.chest then
                        { data | items = chest :: data.items }

                    else
                        case color of
                            ( 255, 255, 255 ) ->
                                { data | player = spawnPlayer coordinate }

                            ( 63, 72, 204 ) ->
                                { data | npcs = addNpc "Dhruv" Orc OpenShopMenu }

                            ( 163, 73, 164 ) ->
                                { data | npcs = addNpc "Villager" Orc SayThings }

                            c ->
                                if c == colors.grass then
                                    data

                                else if c == colors.sandit then
                                    { data
                                        | enemies = addEnemy Sandit
                                        , world = place Sand
                                    }

                                else if c == colors.ragingPig then
                                    { data
                                        | enemies = addEnemy RagingPig
                                        , world = place DarkGrass
                                    }

                                else if c == colors.kelch then
                                    { data
                                        | enemies = addEnemy Kelch
                                        , world = place DarkGrass
                                    }

                                else
                                    Dict.get c colorsToTile
                                        |> Maybe.withDefault (Unknown color)
                                        |> (\t -> { data | world = place t })

                { player, npcs, world, items, enemies } =
                    Bitmap.toDict bitmap
                        |> Dict.foldl loop
                            { player = spawnPlayer ( 0, 0 )
                            , npcs = []
                            , enemies = []
                            , world = Dict.empty
                            , items =
                                [ { kind = Sword, x = 90 * sizes.tile, y = 120 * sizes.tile }
                                ]
                            }
            in
            ( { model
                | world = world
                , player = player
                , npcs = npcs
                , items = items
                , enemies = enemies
              }
            , Cmd.none
            )

        GotWorld _ ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | menu = None }
            , Ports.play
            )

        MouseUp LeftClick ->
            ( { model | mouse = { mouse | leftClick = False } }, Cmd.none )

        MouseUp RightClick ->
            ( { model | mouse = { mouse | rightClick = False } }, Cmd.none )

        MouseDown LeftClick ->
            ( { model | mouse = { mouse | leftClick = True } }, Cmd.none )

        MouseDown RightClick ->
            ( { model | mouse = { mouse | rightClick = True } }, Cmd.none )

        SpritesheetLoaded spritesheet ->
            ( { model | spritesheet = spritesheet }, Cmd.none )

        KeyDown key ->
            ( { model | keys = Set.insert key model.keys }, Cmd.none )

        KeyUp key ->
            let
                menu =
                    if key == keys.menu then
                        case model.menu of
                            MainMenu ->
                                MainMenu

                            ShopMenu ->
                                None

                            PauseMenu ->
                                None

                            None ->
                                PauseMenu

                    else if key == keys.start && model.menu == MainMenu then
                        None

                    else
                        model.menu

                toggleMusicCmd =
                    case ( model.menu, menu ) of
                        ( PauseMenu, _ ) ->
                            Ports.play

                        ( MainMenu, None ) ->
                            Ports.play

                        ( _, PauseMenu ) ->
                            Ports.pause

                        _ ->
                            Cmd.none
            in
            ( { model | keys = Set.remove key model.keys, menu = menu }
            , Cmd.batch
                [ toggleMusicCmd
                , case ( nearbyNpc model, key == keys.interact ) of
                    ( Just (Npc npc), True ) ->
                        if (phases model.phase).npcsEnabled then
                            Task.succeed npc.onSpeak |> Task.perform identity

                        else
                            Cmd.none

                    _ ->
                        Cmd.none
                ]
            )

        Frame dt ->
            let
                player =
                    updatePlayer dt model

                ( pickedUpItems, remainingItems ) =
                    if (phases model.phase).canPickupItems then
                        handleItemPickup player model.items

                    else
                        ( [], model.items )

                enemies_ =
                    model.enemies
                        |> List.filterMap
                            (\e_ ->
                                let
                                    enemy =
                                        updateEnemy dt model e_
                                in
                                if enemy.health < 1 then
                                    Nothing

                                else
                                    Just enemy
                            )

                ( enemies, health ) =
                    enemies_
                        |> List.foldr
                            (\enemy ( list, health_ ) ->
                                ( { enemy | isAttacking = False } :: list
                                , health_ - damageFromEnemy enemy
                                )
                            )
                            ( [], player.health )

                visibilityRadius =
                    7

                ( x, y ) =
                    ( round (player.x / sizes.tile)
                    , round (player.y / sizes.tile)
                    )

                visited =
                    (List.range 0 visibilityRadius |> List.concatMap (\x_ -> List.range 0 visibilityRadius |> List.map (Tuple.pair x_)))
                        |> List.foldl
                            (\( dx, dy ) set ->
                                Set.insert
                                    ( x + dx - visibilityRadius // 2
                                    , y + dy - visibilityRadius // 2
                                    )
                                    set
                            )
                            model.visited

                ticks =
                    model.ticks + dt

                { phase, addedEnemiesThisPhase, addedItemsThisPhase, cmd } =
                    case model.phase of
                        LearningToMove ->
                            { phase =
                                if player.x /= model.player.x || player.y /= model.player.y then
                                    PickingUpSword 0

                                else
                                    model.phase
                            , addedEnemiesThisPhase = []
                            , addedItemsThisPhase = []
                            , cmd = Cmd.none
                            }

                        PickingUpSword time ->
                            { phase =
                                if model.player.hasSword then
                                    LearningToAttack 0

                                else
                                    PickingUpSword (time + dt)
                            , addedEnemiesThisPhase = []
                            , addedItemsThisPhase = []
                            , cmd = Cmd.none
                            }

                        LearningToAttack _ ->
                            if player.isAttacking then
                                let
                                    pig =
                                        spawnEnemy -1 Pig ( 90 * sizes.tile, 125 * sizes.tile )
                                in
                                { phase = FightingFirstEnemies 0
                                , addedEnemiesThisPhase =
                                    [ pig
                                    ]
                                , addedItemsThisPhase = []
                                , cmd = Ports.next
                                }

                            else
                                { phase = model.phase
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Cmd.none
                                }

                        FightingFirstEnemies time ->
                            if List.any (.id >> (==) -1) enemies then
                                { phase = FightingFirstEnemies (time + dt)
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Cmd.none
                                }

                            else
                                { phase = PickingUpTreasure 0
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase =
                                    [ { kind = Gem, x = 90 * sizes.tile, y = 120 * sizes.tile }
                                    ]
                                , cmd = Cmd.none
                                }

                        PickingUpTreasure time ->
                            { phase =
                                if player.gold > 0 then
                                    LearningToInteract 0

                                else
                                    PickingUpTreasure (time + dt)
                            , addedEnemiesThisPhase = []
                            , addedItemsThisPhase = []
                            , cmd = Cmd.none
                            }

                        LearningToInteract time ->
                            if player.hasShield then
                                { phase = LearningToBlock 0
                                , addedEnemiesThisPhase =
                                    [ spawnEnemy -2 RagingPig ( 88 * sizes.tile, 121 * sizes.tile )
                                    ]
                                , addedItemsThisPhase = []
                                , cmd = Cmd.none
                                }

                            else
                                { phase = LearningToInteract (time + dt)
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Cmd.none
                                }

                        LearningToBlock time ->
                            if List.any (.id >> (==) -2) enemies then
                                { phase = LearningToBlock (time + dt)
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Cmd.none
                                }

                            else
                                { phase = ReadyToExplore 0
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Ports.next
                                }

                        ReadyToExplore time ->
                            { phase = ReadyToExplore (time + dt)
                            , addedEnemiesThisPhase = []
                            , addedItemsThisPhase = []
                            , cmd = Cmd.none
                            }

                damageFromEnemy enemy =
                    if enemy.isAttacking && not player.isBlocking && doSquaresCollide (playerToSquare player) (enemyToSquare enemy) then
                        damagePerAttack enemy

                    else
                        0

                kelchIsInView =
                    enemies |> List.any (\enemy -> enemy.kind == Kelch && withinDistance 60 player enemy)

                kelchWasKilled =
                    List.any (\enemy -> enemy.kind == Kelch) model.enemies
                        && not (List.any (\enemy -> enemy.kind == Kelch) enemies)
            in
            ( { model
                | player =
                    { player
                        | gold = pickedUpItems |> List.foldr (\item gold -> goldFromItem item + gold) player.gold
                        , hasSword = player.hasSword || (pickedUpItems |> List.any (.kind >> (==) Sword))
                        , health = health
                        , hasDash = player.hasDash || kelchWasKilled
                    }
                , ticks = ticks
                , items = remainingItems ++ addedItemsThisPhase
                , enemies = enemies ++ addedEnemiesThisPhase
                , visited = visited
                , phase = phase
              }
            , Cmd.batch
                [ cmd
                , if kelchWasKilled then
                    Ports.kelchKilled

                  else if kelchIsInView then
                    Ports.kelchTaunt

                  else
                    Cmd.none
                ]
            )

        OpenShopMenu ->
            ( { model | menu = ShopMenu }, Ports.dhruv )

        SayThings ->
            ( model, Ports.talk )

        CloseShopMenu ->
            ( { model | menu = None }, Cmd.none )

        BoughtShield ->
            ( { model | player = { player_ | hasShield = True }, menu = None }, Cmd.none )


goldFromItem : Item -> Int
goldFromItem item =
    if item.kind == Gem then
        1

    else
        0


poof =
    { frames = 5
    , duration = 600
    }


manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2))


withinDistance n player enemy =
    manhattanDistance ( player.x, player.y ) ( enemy.x, enemy.y ) < n * n * sizes.tile


updateEnemy : Float -> Model -> Enemy -> Enemy
updateEnemy dt { world, player, phase } enemy_ =
    let
        enemy =
            { enemy_
                | attackTimer = max 0 (enemy_.attackTimer - dt)
                , health =
                    if inAttackRange && player.isAttacking then
                        enemy_.health - 1

                    else
                        enemy_.health
            }

        inAttackRange =
            doSquaresCollide
                { x = player.x, y = player.y, size = sizes.player }
                { x = enemy_.x, y = enemy_.y, size = sizes.enemy }

        canSeePlayer =
            withinDistance 35 player enemy
    in
    case enemy.animation of
        Attacking elapsedMs frame ->
            if elapsedMs > durations.enemyAttack then
                { enemy | animation = Idle, attackTimer = durations.enemyAttackDelay, isAttacking = False }

            else
                { enemy | animation = Attacking (elapsedMs + dt) frame }

        _ ->
            if inAttackRange && enemy.kind /= Kelch then
                if not (isPassive enemy) && enemy.attackTimer == 0 then
                    { enemy
                        | animation = Attacking 0 0
                        , isAttacking = True
                    }

                else
                    { enemy | animation = Idle }

            else
                let
                    speed =
                        if enemy.kind == Kelch && withinDistance 35 player enemy then
                            -0.2 * dt

                        else if enemy.kind == Kelch && withinDistance 37 player enemy then
                            0

                        else if enemy.kind == Kelch && withinDistance 60 player enemy then
                            0.2 * dt

                        else if canSeePlayer then
                            0.12 * dt

                        else
                            0

                    moveTowardPlayer :
                        (Player -> Float)
                        -> (Enemy -> Float)
                        -> Float
                    moveTowardPlayer f g =
                        if floor (f player - g enemy) < 0 then
                            -1

                        else if floor (f player - g enemy) > 0 then
                            1

                        else
                            0

                    ( dx, dy ) =
                        ( moveTowardPlayer .x .x
                        , moveTowardPlayer .y .y
                        )
                            |> normalize
                            |> Tuple.mapBoth ((*) speed) ((*) speed)

                    animation =
                        if newX == enemy.x && newY == enemy.y then
                            Idle

                        else
                            Running

                    direction =
                        if dx < 0 then
                            Left

                        else if dx > 0 then
                            Right

                        else
                            enemy.direction

                    ( newX, newY ) =
                        attemptMovement phase sizes.enemy world enemy ( dx, dy )

                    isStuck =
                        ( newX, newY ) == ( enemy.x, enemy.y )
                in
                if enemy.kind == Kelch && withinDistance 5 player enemy && isStuck then
                    { enemy
                        | direction = direction
                        , animation = animation
                        , x = player.x - dx
                        , y = player.y - dy
                        , timeSpentStuck = 0
                    }

                else
                    { enemy | direction = direction, animation = animation, x = newX, y = newY, timeSpentStuck = 0 }



-- else
--     Just { enemy | animation = Idle }


sizes =
    { player = 64
    , npc = 64
    , enemy = 64
    , item = 48
    , tile = 64
    , fireball = 32
    }


handleItemPickup : Player -> List Item -> ( List Item, List Item )
handleItemPickup player items =
    List.partition
        (\{ x, y } ->
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
    , ability : String
    , start : String
    , dash : String
    }
keys =
    { left = "left"
    , right = "right"
    , up = "up"
    , down = "down"
    , menu = "menu"
    , interact = "interact"
    , ability = "ability"
    , start = "start"
    , dash = "dash"
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


attackAnimation :
    { fps : number
    , frames : number
    , attackFrame : number
    , delay : number
    }
attackAnimation =
    { fps = 12
    , frames = 5
    , attackFrame = 3
    , delay = 500
    }


fireballAttack =
    { speed = 0.2
    , duration = 2000
    }


updatePlayer : Float -> Model -> Player
updatePlayer dt ({ mouse, player } as model) =
    let
        speed =
            if player.dashElapsed < dash.cooldown && player.dashElapsed > dash.duration then
                dt * 0.5

            else
                dt * 0.25

        ( dx, dy ) =
            Set.foldl move ( 0, 0 ) model.keys
                |> normalize
                |> Tuple.mapBoth ((*) speed) ((*) speed)

        isHoldingAttack =
            mouse.leftClick

        isDashing =
            Set.member keys.dash model.keys
                && (player.dashElapsed == 0)
                && player.hasDash

        inAttackAnimationFrame : Bool
        inAttackAnimationFrame =
            case player.animation of
                Attacking elapsedMs _ ->
                    getAttackFrame elapsedMs == attackAnimation.attackFrame

                _ ->
                    False

        getAttackFrame : Float -> Int
        getAttackFrame elapsedMs =
            round (elapsedMs / 1000 * attackAnimation.fps)

        isHoldingAbility =
            Set.member keys.ability model.keys

        isShootingFireball =
            not isBlocking && model.player.attackTimer == 0 && isHoldingAbility && hasFireball player

        animation : Animation
        animation =
            case player.animation of
                Attacking elapsedMs _ ->
                    if elapsedMs < 1000 * attackAnimation.frames / attackAnimation.fps then
                        Attacking (elapsedMs + dt) (getAttackFrame elapsedMs)

                    else
                        checkAnimation

                _ ->
                    checkAnimation

        isAttackingThisTurn =
            not isBlocking && model.player.attackTimer == 0 && isHoldingAttack && player.hasSword

        isBlocking =
            mouse.rightClick

        checkAnimation =
            if isBlocking then
                Blocking

            else if isAttackingThisTurn then
                Attacking 0 0

            else if ( dx, dy ) == ( 0, 0 ) then
                Idle

            else
                Running

        ( newX, newY ) =
            if isBlocking then
                attemptMovement model.phase sizes.player model.world player ( dx / 4, dy / 4 )

            else
                attemptMovement model.phase sizes.player model.world player ( dx, dy )

        attackTimer =
            max 0 (model.player.attackTimer - dt)

        fireball =
            model.player.fireball
                |> Maybe.map
                    (\f ->
                        { f
                            | duration = max 0 (f.duration - dt)
                            , x = f.vx * dt * fireballAttack.speed + f.x
                            , target = model.enemies |> List.filter (\e -> doSquaresCollide (enemyToSquare e) (projectileToSquare f)) |> List.head
                        }
                    )

        direction =
            if Set.member keys.left model.keys then
                Left

            else if Set.member keys.right model.keys then
                Right

            else
                player.direction
    in
    { player
        | x = newX
        , y = newY
        , direction = direction
        , animation = animation
        , attackTimer =
            if isAttackingThisTurn then
                attackAnimation.delay

            else
                attackTimer
        , isAttacking = inAttackAnimationFrame
        , isBlocking = isBlocking
        , dashElapsed =
            max
                (if isDashing then
                    dash.cooldown

                 else
                    player.dashElapsed - dt
                )
                0
        , fireball =
            case player.fireball of
                Nothing ->
                    if isShootingFireball then
                        Just
                            { x = newX
                            , y = newY
                            , vx =
                                case direction of
                                    Left ->
                                        -1

                                    Right ->
                                        1
                            , vy = 0
                            , duration = fireballAttack.duration
                            , target = Nothing
                            }

                    else
                        Nothing

                Just f ->
                    if f.duration == 0 || f.target /= Nothing then
                        Nothing

                    else
                        fireball
    }


attemptMovement :
    Phase
    -> Float
    -> Dict ( Int, Int ) Tile
    -> { a | x : Float, y : Float }
    -> ( Float, Float )
    -> ( Float, Float )
attemptMovement phase size world mob ( dx, dy ) =
    let
        quarter =
            size / 4

        hasCollision ( x, y ) =
            List.any collideWithTerrain
                [ ( x + quarter, y + size - quarter )
                , ( x + size - quarter, y + size - quarter )
                , ( x + quarter, y + size )
                , ( x + size - quarter, y + size )
                ]

        isWalkable : Tile -> Bool
        isWalkable tile =
            List.member tile (walkable phase)

        collideWithTerrain : ( Float, Float ) -> Bool
        collideWithTerrain ( x, y ) =
            Dict.get
                ( floor (x / sizes.tile)
                , floor (y / sizes.tile)
                )
                world
                |> Maybe.map (isWalkable >> not)
                |> Maybe.withDefault False
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
            , case model.menu of
                None ->
                    [ Browser.Events.onAnimationFrameDelta Frame
                    , Browser.Events.onMouseDown (mouseDecoderFor MouseDown)
                    , Browser.Events.onMouseUp (mouseDecoderFor MouseUp)
                    ]

                MainMenu ->
                    []

                PauseMenu ->
                    []

                ShopMenu ->
                    []
            ]


keyCodeMapping : Dict String Key
keyCodeMapping =
    Dict.fromList
        [ ( "KeyA", keys.left )
        , ( "KeyD", keys.right )
        , ( "KeyW", keys.up )
        , ( "KeyS", keys.down )
        , ( "Escape", keys.menu )
        , ( "Enter", keys.start )
        , ( "KeyE", keys.interact )
        , ( "KeyQ", keys.ability )
        , ( "Space", keys.dash )
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


mouseDecoderFor : (MouseButton -> Msg) -> Json.Decoder Msg
mouseDecoderFor toMsg =
    let
        toMouseButtonMsg int =
            case int of
                0 ->
                    Json.succeed (toMsg LeftClick)

                2 ->
                    Json.succeed (toMsg RightClick)

                _ ->
                    Json.fail "Unrecognized mouse button"
    in
    Json.field "button" Json.int
        |> Json.andThen toMouseButtonMsg



-- VIEW


type alias Item =
    { kind : ItemKind
    , x : Float
    , y : Float
    }


type ItemKind
    = Sword
    | Gem


type alias Square =
    { x : Float, y : Float, size : Float }


npcToSquare : Npc -> Square
npcToSquare (Npc npc) =
    { x = npc.x
    , y = npc.y
    , size = sizes.npc
    }


projectileToSquare : Projectile -> Square
projectileToSquare p =
    { x = p.x
    , y = p.y
    , size = sizes.fireball
    }


enemyToSquare : Enemy -> Square
enemyToSquare p =
    { x = p.x
    , y = p.y
    , size = sizes.enemy
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


colorFromTuple ( r, g, b ) =
    Color.rgb255 r g b


type alias PhaseEffect =
    { whiteGrass : Bool
    , rectangleWorld : Bool
    , blackAndWhiteHero : Bool
    , npcsEnabled : Bool
    , canPickupItems : Bool
    , canUseBridge : Bool
    , canTakeDamage : Bool
    , canViewGold : Bool
    }


phases : Phase -> PhaseEffect
phases phase =
    case phase of
        LearningToMove ->
            { whiteGrass = True
            , rectangleWorld = True
            , blackAndWhiteHero = True
            , npcsEnabled = False
            , canPickupItems = False
            , canUseBridge = False
            , canTakeDamage = False
            , canViewGold = False
            }

        PickingUpSword _ ->
            { whiteGrass = True
            , rectangleWorld = True
            , blackAndWhiteHero = False
            , npcsEnabled = False
            , canPickupItems = True
            , canUseBridge = False
            , canTakeDamage = False
            , canViewGold = False
            }

        LearningToAttack _ ->
            { whiteGrass = False
            , rectangleWorld = True
            , blackAndWhiteHero = False
            , npcsEnabled = False
            , canPickupItems = True
            , canUseBridge = False
            , canTakeDamage = False
            , canViewGold = False
            }

        FightingFirstEnemies _ ->
            { whiteGrass = False
            , rectangleWorld = True
            , blackAndWhiteHero = False
            , npcsEnabled = False
            , canPickupItems = True
            , canUseBridge = False
            , canTakeDamage = False
            , canViewGold = False
            }

        PickingUpTreasure _ ->
            { whiteGrass = False
            , rectangleWorld = True
            , blackAndWhiteHero = False
            , npcsEnabled = False
            , canPickupItems = True
            , canUseBridge = False
            , canTakeDamage = False
            , canViewGold = False
            }

        LearningToInteract _ ->
            { whiteGrass = False
            , rectangleWorld = False
            , blackAndWhiteHero = False
            , npcsEnabled = True
            , canPickupItems = True
            , canUseBridge = False
            , canTakeDamage = False
            , canViewGold = True
            }

        LearningToBlock _ ->
            { whiteGrass = False
            , rectangleWorld = False
            , blackAndWhiteHero = False
            , npcsEnabled = True
            , canPickupItems = True
            , canUseBridge = False
            , canTakeDamage = True
            , canViewGold = True
            }

        ReadyToExplore _ ->
            { whiteGrass = False
            , rectangleWorld = False
            , blackAndWhiteHero = False
            , npcsEnabled = True
            , canPickupItems = True
            , canUseBridge = True
            , canTakeDamage = True
            , canViewGold = True
            }


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Unblank"
    , body =
        [ Html.div
            [ Attr.class "game"
            , Attr.classList
                [ ( "black-and-white", (phases model.phase).blackAndWhiteHero )
                , ( "hide-game", model.menu == MainMenu )
                ]
            ]
            [ Elm2D.viewFollowCamera
                { background =
                    if (phases model.phase).whiteGrass then
                        Color.white

                    else
                        colorFromTuple colors.grass
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
                        viewGame spritesheet model
                )
            , case nearbyNpc model of
                Just (Npc npc) ->
                    if (phases model.phase).npcsEnabled then
                        Html.div [ Attr.class "dialogue-prompt" ]
                            [ Html.text "Talk to "
                            , Html.strong [] [ Html.text npc.name ]
                            ]

                    else
                        Html.text ""

                Nothing ->
                    Html.text ""
            , viewHud model
            , Html.div []
                (if model.menu /= MainMenu then
                    case model.phase of
                        LearningToMove ->
                            [ Html.div [ Attr.class "tutorial-prompt__wrapper" ]
                                [ Html.div [ Attr.class "tutorial-prompt" ]
                                    [ Html.text "WASD to move"
                                    ]
                                ]
                            ]

                        PickingUpSword _ ->
                            []

                        LearningToAttack _ ->
                            [ Html.div [ Attr.class "tutorial-prompt__wrapper" ]
                                [ Html.div [ Attr.class "tutorial-prompt" ]
                                    [ Html.text "Left-click to attack"
                                    ]
                                ]
                            ]

                        FightingFirstEnemies _ ->
                            []

                        PickingUpTreasure _ ->
                            [ Html.div [ Attr.class "tutorial-prompt__wrapper" ]
                                [ Html.div [ Attr.class "tutorial-prompt" ]
                                    [ Html.text "Gems are hidden everywhere"
                                    ]
                                ]
                            ]

                        LearningToInteract _ ->
                            []

                        LearningToBlock _ ->
                            [ Html.div [ Attr.class "tutorial-prompt__wrapper" ]
                                [ Html.div [ Attr.class "tutorial-prompt" ]
                                    [ Html.text "Right-click to block"
                                    ]
                                ]
                            ]

                        ReadyToExplore time ->
                            if time > 1000 && time < 4000 then
                                [ Html.div [ Attr.class "tutorial-prompt__wrapper" ]
                                    [ Html.div [ Attr.class "tutorial-prompt" ]
                                        [ Html.text "Ooh, a bridge!"
                                        ]
                                    ]
                                ]

                            else
                                []

                 else
                    []
                )
            , case model.menu of
                MainMenu ->
                    Html.div [ Attr.class "overlay main-menu" ]
                        [ Html.div [ Attr.class "main pause" ]
                            [ Html.h3 [ Attr.class "title" ] [ Html.text "Unblank" ]
                            , Html.button [ Attr.class "btn", Html.Events.onClick StartGame ] [ Html.text "Play" ]
                            ]
                        ]

                PauseMenu ->
                    Html.div [ Attr.class "overlay" ]
                        [ Html.div [ Attr.class "pause" ]
                            [ Html.h3 [ Attr.class "title" ] [ Html.text "Unblank" ]
                            , Html.div [] [ Html.text "Press ESC to continue" ]
                            ]
                        ]

                ShopMenu ->
                    Html.div [ Attr.class "overlay" ]
                        [ Html.button [ Attr.class "shop__close", Html.Events.onClick CloseShopMenu ] []
                        , Html.section [ Attr.class "shop" ]
                            [ Html.h3 [ Attr.class "title" ] [ Html.text "Dhruv's Magical Shield Shack" ]
                            , if model.player.hasShield then
                                Html.div [ Attr.class "shop__empty" ] [ Html.text "( Sold out! )" ]

                              else
                                Html.button [ Attr.class "item", Html.Events.onClick BoughtShield ]
                                    [ Html.div [ Attr.class "item__image", Attr.style "background-image" "url('/images/shield_16.png')" ] []
                                    , Html.div [ Attr.class "item__name" ] [ Html.text "Shield" ]
                                    , Html.div [ Attr.class "item__details" ] [ Html.text "You really want this thing." ]
                                    ]
                            ]
                        ]

                None ->
                    Html.text ""
            ]
        ]
    }


poofAnimation : Float -> Spritesheet -> Sprite
poofAnimation elapsed spritesheet =
    Elm2D.Spritesheet.frame (modBy poof.frames (round (elapsed / poof.duration * poof.frames)))
        (Elm2D.Spritesheet.animation spritesheet
            [ ( 9, 11 )
            , ( 9, 12 )
            , ( 9, 12 )
            , ( 9, 11 )
            , ( 9, 10 )
            ]
        )


viewGame : Spritesheet -> Model -> List Elm2D.Element
viewGame spritesheet model =
    let
        sprites =
            { sword = ( 0, 8 )
            , gem = ( 6, 8 )
            }

        viewItem item =
            let
                sprite =
                    case item.kind of
                        Sword ->
                            sprites.sword

                        Gem ->
                            sprites.gem
            in
            Elm2D.sprite
                { sprite =
                    case model.phase of
                        PickingUpSword elapsed ->
                            if elapsed > poof.duration then
                                Elm2D.Spritesheet.select spritesheet sprite

                            else
                                poofAnimation elapsed spritesheet

                        PickingUpTreasure elapsed ->
                            if elapsed > poof.duration then
                                Elm2D.Spritesheet.select spritesheet sprite

                            else
                                poofAnimation elapsed spritesheet

                        _ ->
                            Elm2D.Spritesheet.select spritesheet sprite
                , size = ( sizes.item, sizes.item )
                , position =
                    ( item.x + ((sizes.tile - sizes.item) / 2)
                    , item.y - (4 * sin (0.004 * model.ticks)) + ((sizes.tile - sizes.item) / 2)
                    )
                }

        toWorldPosition xy =
            Tuple.mapBoth
                (toFloat >> (*) sizes.tile)
                (toFloat >> (*) sizes.tile)
                xy

        viewWorldTile : ( Int, Int ) -> Tile -> Elm2D.Element
        viewWorldTile xy tile =
            let
                size =
                    ( sizes.tile, sizes.tile )

                position =
                    toWorldPosition xy
            in
            if (phases model.phase).rectangleWorld then
                Elm2D.rectangle
                    { size = size
                    , position = position
                    , color =
                        if List.member tile (walkable model.phase) then
                            colorFromTuple colors.grass

                        else
                            Color.rgb255 50 50 50
                    }

            else
                case tile of
                    Tree ->
                        Elm2D.sprite
                            { sprite = Elm2D.Spritesheet.select spritesheet ( 4, 8 )
                            , size = size
                            , position = position
                            }

                    Water ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = colorFromTuple colors.water
                            }

                    Bridge ->
                        let
                            bridge =
                                Elm2D.rectangle
                                    { size = size
                                    , position = position
                                    , color =
                                        if (phases model.phase).canUseBridge then
                                            colorFromTuple colors.bridge

                                        else
                                            colorFromTuple colors.water
                                    }
                        in
                        case model.phase of
                            ReadyToExplore elapsed ->
                                if elapsed > poof.duration then
                                    bridge

                                else
                                    Elm2D.sprite
                                        { size = size
                                        , position = position
                                        , sprite = poofAnimation elapsed spritesheet
                                        }

                            _ ->
                                bridge

                    UnderwaterBridge ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = colorFromTuple colors.water
                            }

                    Sand ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = colorFromTuple colors.sand
                            }

                    Gravel ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = colorFromTuple colors.gravel
                            }

                    ColdLava ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = colorFromTuple colors.coldLava
                            }

                    DarkGrass ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = colorFromTuple colors.darkGrass
                            }

                    Unknown ( r, g, b ) ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = Color.rgb255 r g b
                            }

        visibleRange : List ( Int, Int )
        visibleRange =
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
                |> List.concatMap (\x -> List.range bounds.top bottom |> List.map (Tuple.pair x))

        tiles : List Elm2D.Element
        tiles =
            visibleRange
                |> List.filterMap
                    (\xy -> Dict.get xy model.world |> Maybe.map (viewWorldTile xy))

        viewFireball : Projectile -> Elm2D.Element
        viewFireball fireball =
            let
                col =
                    if fireball.vx > 0 then
                        6

                    else
                        7
            in
            Elm2D.sprite
                { size = ( sizes.fireball, sizes.fireball )
                , position = ( fireball.x + ((sizes.tile - sizes.fireball) / 2), fireball.y + ((sizes.tile - sizes.fireball) / 2) )
                , sprite =
                    Elm2D.Spritesheet.frame (modBy 3 (round model.ticks // 100))
                        (Elm2D.Spritesheet.animation spritesheet [ ( col, 10 ), ( col, 11 ), ( col, 12 ) ])
                }

        viewFog : ( Int, Int ) -> Maybe Elm2D.Element
        viewFog xy =
            if Set.member xy model.visited then
                Nothing

            else
                Just
                    (Elm2D.rectangle
                        { size = ( sizes.tile, sizes.tile )
                        , position = toWorldPosition xy
                        , color = Color.white
                        }
                    )

        layers =
            { items =
                if (phases model.phase).canPickupItems then
                    List.map viewItem model.items

                else
                    []
            , enemies = List.map (viewEnemy spritesheet model) model.enemies
            , npcs = List.map (viewNpc spritesheet model) model.npcs
            , player =
                [ ( model.player.y
                  , Elm2D.sprite
                        { sprite = viewPlayer spritesheet model
                        , size = ( sizes.player, sizes.player )
                        , position = ( model.player.x, model.player.y )
                        }
                  )
                ]
            , projectiles = List.filterMap (Maybe.map viewFireball) [ model.player.fireball ]
            , fog = List.filterMap viewFog visibleRange
            }
    in
    List.concat
        [ tiles
        , layers.items
        , List.concat
            [ layers.enemies
            , layers.npcs
            , layers.player
            ]
            |> List.sortBy Tuple.first
            |> List.map Tuple.second
        , layers.projectiles

        -- , layers.fog
        ]


hasFireball : Player -> Bool
hasFireball player =
    False



-- TODO: Remove this


viewPlayer : Spritesheet -> { model | ticks : Float, player : Player } -> Sprite
viewPlayer spritesheet model =
    let
        itemOffset =
            if model.player.hasShield then
                4

            else if model.player.hasSword then
                2

            else
                0

        col =
            case model.player.direction of
                Right ->
                    8 + itemOffset

                Left ->
                    9 + itemOffset
    in
    case model.player.animation of
        Idle ->
            Elm2D.Spritesheet.select spritesheet ( col, 0 )

        Running ->
            Elm2D.Spritesheet.frame (modBy 4 (round model.ticks // 200))
                (Elm2D.Spritesheet.animation spritesheet [ ( col, 1 ), ( col, 0 ), ( col, 2 ), ( col, 0 ) ])

        Blocking ->
            Elm2D.Spritesheet.select spritesheet ( col, 3 )

        Attacking _ frame ->
            Elm2D.Spritesheet.frame frame
                (Elm2D.Spritesheet.animation spritesheet [ ( col, 4 ), ( col, 5 ), ( col, 6 ), ( col, 7 ), ( col, 8 ) ])


durations =
    { enemyAttack = 200
    , enemyAttackDelay = 1000
    }


viewEnemy : Spritesheet -> Model -> Enemy -> ( Float, Elm2D.Element )
viewEnemy spritesheet model ({ x, y } as enemy) =
    let
        col =
            case enemy.direction of
                Left ->
                    1

                Right ->
                    0

        row =
            case enemy.kind of
                Pig ->
                    10

                RagingPig ->
                    14

                Sandit ->
                    18

                Kelch ->
                    23

        size =
            ( sizes.enemy, sizes.enemy )

        position =
            case enemy.animation of
                Attacking ms _ ->
                    ( (cos (ms / durations.enemyAttack / 2) * (model.player.x - x) / 4) + x
                    , (cos (ms / durations.enemyAttack / 2) * (model.player.y - y) / 4) + y
                    )

                _ ->
                    ( x, y )

        enemySprite =
            case enemy.animation of
                Idle ->
                    Elm2D.Spritesheet.select spritesheet ( col, row )

                Running ->
                    Elm2D.Spritesheet.frame (modBy 3 (round model.ticks // 150))
                        (Elm2D.Spritesheet.animation spritesheet [ ( col, row ), ( col, row + 1 ), ( col, row + 2 ) ])

                Attacking _ _ ->
                    Elm2D.Spritesheet.select spritesheet ( col, row + 3 )

                Blocking ->
                    Elm2D.Spritesheet.select spritesheet ( col, row )
    in
    ( y
    , Elm2D.sprite
        { size = size
        , position = position
        , sprite =
            case model.phase of
                FightingFirstEnemies elapsed ->
                    if elapsed > poof.duration then
                        enemySprite

                    else
                        poofAnimation elapsed spritesheet

                _ ->
                    enemySprite
        }
    )


npcSpriteMapping col =
    Dict.fromList
        [ ( "Dhruv", ( 3 + col, 14 ) )
        ]


viewNpc : Spritesheet -> Model -> Npc -> ( Float, Elm2D.Element )
viewNpc spritesheet model (Npc npc) =
    let
        col =
            case npc.direction of
                Right ->
                    0

                Left ->
                    1

        sprite =
            Dict.get npc.name (npcSpriteMapping col)
                |> Maybe.withDefault ( col + 3, 10 )
    in
    if (phases model.phase).npcsEnabled then
        case model.phase of
            LearningToInteract elapsed ->
                if elapsed > poof.duration then
                    ( npc.y
                    , Elm2D.sprite
                        { size = ( sizes.npc, sizes.npc )
                        , position = ( npc.x, npc.y )
                        , sprite = Elm2D.Spritesheet.select spritesheet sprite
                        }
                    )

                else
                    ( 0
                    , Elm2D.sprite
                        { sprite = poofAnimation elapsed spritesheet
                        , size = ( sizes.npc, sizes.npc )
                        , position = ( npc.x, npc.y )
                        }
                    )

            _ ->
                ( npc.y
                , Elm2D.sprite
                    { size = ( sizes.npc, sizes.npc )
                    , position = ( npc.x, npc.y )
                    , sprite = Elm2D.Spritesheet.select spritesheet sprite
                    }
                )

    else
        ( 0
        , Elm2D.rectangle
            { size = ( sizes.npc, sizes.npc )
            , position = ( npc.x, npc.y )
            , color =
                if (phases model.phase).whiteGrass then
                    Color.white

                else
                    colorFromTuple colors.grass
            }
        )


viewHud : Model -> Html Msg
viewHud model =
    Html.div [ Attr.class "hud" ]
        [ if (phases model.phase).canViewGold then
            Html.div [ Attr.class "gold" ]
                [ Html.text (model.player.gold |> String.fromInt)
                ]

          else
            Html.text ""
        , if (phases model.phase).canTakeDamage then
            Html.progress
                [ Attr.class "healthbar"
                , Attr.value
                    (if model.player.health == model.player.maxHealth then
                        "1"

                     else
                        (toFloat model.player.health / toFloat model.player.maxHealth) |> String.fromFloat
                    )
                ]
                []

          else
            Html.text ""
        ]
