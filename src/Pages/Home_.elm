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
    , world : World
    , mouse : MouseState
    , keys : Set Key
    , ticks : Float
    , visited : Set ( Int, Int )
    }


type alias World =
    { player : Player
    , items : List Item
    , enemies : List Enemy
    , terrain : Terrain
    , npcs : List Npc
    }


type alias Terrain =
    Dict ( Int, Int ) Tile


type alias Npc =
    { kind : NpcKind
    , x : Float
    , y : Float
    , direction : Direction
    }


type NpcKind
    = Dhruv
    | Scott
    | SnootyLady
    | FanBoy
    | Blacksmith


nameOfNpc : Npc -> String
nameOfNpc npc =
    case npc.kind of
        Dhruv ->
            "Dhruv"

        Scott ->
            "Scott"

        SnootyLady ->
            "lady"

        FanBoy ->
            "boy"

        Blacksmith ->
            "Blacksmith"


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

        Nick ->
            False

        Null ->
            True


type EnemyKind
    = Pig
    | RagingPig
    | Sandit
    | Kelch
    | Nick
    | Null


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

        Nick ->
            2

        Null ->
            0


type Tile
    = Tree
    | Water
    | Bridge
    | Sand
    | DarkGrass
    | Gravel
    | ColdLava
    | Void
    | Gate
    | VillageGate
    | ForestGate
    | DesertGate
    | VolcanoGate
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
    , dashElapsed : Float
    , health : Int
    , maxHealth : Int
    , hasSword : Bool
    , hasShield : Bool
    , hasDash : Bool
    , hasFireball : Bool
    , hasArtifact : Bool
    , hasGateKey : Bool
    , gems : GemInventory
    }


type alias GemInventory =
    { village : Int
    , forest : Int
    , desert : Int
    , volcano : Int
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
    | FightingBoss Float
    | GameOver Float


init : ( Model, Cmd Msg )
init =
    ( { phase = LearningToMove
      , menu = MainMenu
      , spritesheet = Nothing
      , ticks = 0
      , keys = Set.empty
      , mouse = { leftClick = False, rightClick = False }
      , visited = Set.empty
      , world =
            { player = spawnPlayer ( 0, 0 )
            , enemies = []
            , terrain = Dict.empty
            , items =
                [ { kind = Sword, x = 90 * sizes.tile, y = 120 * sizes.tile }
                ]
            , npcs = []
            }
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
    | ScottSpeak
    | SnootyLadySpeak
    | FanBoySpeak
    | BlacksmithSpeak
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


spawnPlayer : ( Float, Float ) -> Player
spawnPlayer ( x, y ) =
    { x = x
    , y = y
    , direction = Left
    , animation = Idle
    , attackTimer = 0
    , isAttacking = False
    , isBlocking = False
    , fireball = Nothing
    , gems =
        { village = 0
        , forest = 0
        , desert = 0
        , volcano = 0
        }
    , health = 10
    , maxHealth = 10
    , hasSword = False
    , hasShield = False
    , hasDash = False
    , hasGateKey = False
    , hasArtifact = False
    , hasFireball = False
    , dashElapsed = 0
    }


dash =
    { cooldown = 1000
    , duration = 300
    }


spawnEnemy : Int -> EnemyKind -> ( Float, Float ) -> Enemy
spawnEnemy id kind ( x, y ) =
    case kind of
        Kelch ->
            Enemy id kind Right Idle x y 50 0 False 0

        Nick ->
            Enemy id kind Left Idle x y 50 0 False 0

        _ ->
            Enemy id kind Right Idle x y 3 0 False 0


colors =
    { bridge = ( 185, 122, 87 )
    , underwaterBridge = ( 112, 146, 190 )
    , sand = ( 239, 228, 176 )
    , gravel = ( 195, 195, 195 )
    , darkGrass = ( 106, 159, 124 )
    , tree = ( 21, 111, 48 )
    , grass = ( 130, 190, 150 )
    , coldLava = ( 136, 11, 17 )
    , water = ( 140, 211, 232 )
    , gardenGate = ( 53, 63, 79 )
    , void = ( 0, 0, 0 )
    , artifact = ( 255, 201, 14 )
    , village =
        { gem = ( 0, 162, 232 )
        , gate = ( 0, 0, 160 )
        }
    , forest =
        { gem = ( 0, 255, 0 )
        , gate = ( 0, 64, 64 )
        , enemy = ( 237, 28, 36 )
        }
    , desert =
        { gem = ( 250, 200, 50 )
        , gate = ( 169, 133, 22 )
        , enemy = ( 128, 64, 0 )
        }
    , volcano =
        { gem = ( 255, 0, 255 )
        , gate = ( 128, 0, 255 )
        , enemy = ( 234, 157, 159 )
        }
    , characters =
        { player = ( 255, 255, 255 )
        , dhruv = ( 63, 72, 204 )
        , kelch = ( 64, 0, 128 )
        , nick = ( 142, 13, 120 )
        , scott = ( 170, 170, 85 )
        , lady = ( 163, 73, 164 )
        , kid = ( 34, 113, 111 )
        , blacksmith = ( 124, 67, 95 )
        }
    }


fromBitmapColor : Dict ( Int, Int, Int ) (( Int, Int ) -> World -> World)
fromBitmapColor =
    Dict.fromList
        [ ( colors.grass, \_ world -> world )
        , ( colors.bridge, addTile Bridge )
        , ( colors.sand, addTile Sand )
        , ( colors.gravel, addTile Gravel )
        , ( colors.coldLava, addTile ColdLava )
        , ( colors.darkGrass, addTile DarkGrass )
        , ( colors.tree, addTile Tree )
        , ( colors.gardenGate, addTile Gate )
        , ( colors.water, addTile Water )
        , ( colors.characters.kelch, addEnemyOn Kelch DarkGrass )
        , ( colors.characters.dhruv, addNpc Dhruv )
        , ( colors.characters.player, addPlayer )
        , ( colors.characters.nick, addEnemyOn Nick Sand )
        , ( colors.characters.scott, addNpc Scott )
        , ( colors.characters.lady, addNpc SnootyLady )
        , ( colors.characters.kid, addNpc FanBoy )
        , ( colors.characters.blacksmith, addNpc Blacksmith )
        , ( colors.village.gem, addItemOn VillageGem Nothing )
        , ( colors.village.gate, addTile VillageGate )
        , ( colors.forest.gem, addItemOn ForestGem (Just DarkGrass) )
        , ( colors.forest.enemy, addEnemyOn RagingPig DarkGrass )
        , ( colors.forest.gate, addTile ForestGate )
        , ( colors.desert.gem, addItemOn DesertGem (Just Sand) )
        , ( colors.desert.enemy, addEnemyOn Sandit Sand )
        , ( colors.desert.gate, addTile DesertGate )
        , ( colors.volcano.gem, addItemOn VolcanoGem (Just Gravel) )
        , ( colors.volcano.gate, addTile VolcanoGate )
        , ( colors.volcano.enemy, addEnemyOn RagingPig Gravel )
        , ( colors.artifact, addItemOn Artifact (Just Gravel) )
        ]


fromCoordinates : ( Int, Int ) -> ( Float, Float )
fromCoordinates =
    Tuple.mapBoth fromIndex fromIndex


fromIndex : Int -> Float
fromIndex a =
    toFloat a * sizes.tile


addTile : Tile -> ( Int, Int ) -> World -> World
addTile tile coords world =
    { world | terrain = Dict.insert coords tile world.terrain }


addNpc : NpcKind -> ( Int, Int ) -> World -> World
addNpc kind ( x, y ) world =
    { world | npcs = Npc kind (fromIndex x) (fromIndex y) Left :: world.npcs }


addEnemyOn : EnemyKind -> Tile -> ( Int, Int ) -> World -> World
addEnemyOn kind tile coords world =
    { world | enemies = spawnEnemy (List.length world.enemies) kind (fromCoordinates coords) :: world.enemies }
        |> addTile tile coords


addItemOn : ItemKind -> Maybe Tile -> ( Int, Int ) -> World -> World
addItemOn kind maybeTile ( x, y ) world =
    { world | items = Item kind (fromIndex x) (fromIndex y) :: world.items }
        |> (maybeTile |> Maybe.map (\t -> addTile t ( x, y )) |> Maybe.withDefault identity)


addPlayer : ( Int, Int ) -> World -> World
addPlayer coords world =
    { world | player = spawnPlayer (fromCoordinates coords) }


createFromBitmap : ( Int, Int ) -> ( Int, Int, Int ) -> World -> World
createFromBitmap ( x, y ) color world =
    let
        fn =
            Dict.get color fromBitmapColor
                |> Maybe.withDefault (addTile (Unknown color))
    in
    fn ( x, y ) world


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { mouse, world } =
            model

        player_ =
            world.player
    in
    case msg of
        GotWorld (Ok bitmap) ->
            ( { model
                | world =
                    bitmap
                        |> Bitmap.toDict
                        |> Dict.foldl createFromBitmap model.world
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

                worldIfUnlockingDoor =
                    if key == keys.interact then
                        if player_.hasGateKey && aboveGate world then
                            { world | terrain = Dict.remove (tileBelowPlayer world.player) world.terrain }

                        else if canUnlockLowerGates world then
                            { world | terrain = Dict.filter (\_ value -> not (List.member value [ VillageGate, DesertGate, ForestGate ])) world.terrain }

                        else if canUnlockVolcanoGate world then
                            { world
                                | terrain =
                                    Dict.map
                                        (\_ value ->
                                            if value == VolcanoGate then
                                                Gravel

                                            else
                                                value
                                        )
                                        world.terrain
                            }

                        else
                            world

                    else
                        world
            in
            ( { model
                | keys = Set.remove key model.keys
                , menu = menu
                , world = worldIfUnlockingDoor
              }
            , Cmd.batch
                [ toggleMusicCmd
                , case ( nearbyNpc model.world, key == keys.interact ) of
                    ( Just npc, True ) ->
                        if (phases model.phase).npcsEnabled then
                            (Task.perform identity << Task.succeed)
                                (case npc.kind of
                                    Dhruv ->
                                        OpenShopMenu

                                    Scott ->
                                        ScottSpeak

                                    SnootyLady ->
                                        SnootyLadySpeak

                                    FanBoy ->
                                        FanBoySpeak

                                    Blacksmith ->
                                        BlacksmithSpeak
                                )

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
                        handleItemPickup player world.items

                    else
                        ( [], world.items )

                enemies_ =
                    world.enemies
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
                    playerCoordinates player

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
                                if player.x /= world.player.x || player.y /= world.player.y then
                                    PickingUpSword 0

                                else
                                    model.phase
                            , addedEnemiesThisPhase = []
                            , addedItemsThisPhase = []
                            , cmd = Cmd.none
                            }

                        PickingUpSword time ->
                            { phase =
                                if world.player.hasSword then
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
                                    [ { kind = VillageGem, x = 90 * sizes.tile, y = 120 * sizes.tile }
                                    ]
                                , cmd = Cmd.none
                                }

                        PickingUpTreasure time ->
                            { phase =
                                if player.gems.village > 0 then
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
                            if justPickedUp Artifact then
                                { phase = FightingBoss 0
                                , addedEnemiesThisPhase =
                                    [ spawnEnemy 9000 Null ( 127 * sizes.tile, 28 * sizes.tile )
                                    ]
                                , addedItemsThisPhase = []
                                , cmd = Ports.bossIntro
                                }

                            else
                                { phase = ReadyToExplore (time + dt)
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Cmd.none
                                }

                        FightingBoss time ->
                            if wasKilled Null then
                                { phase = GameOver 0
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Ports.bossKilled
                                }

                            else
                                { phase = FightingBoss (time + dt)
                                , addedEnemiesThisPhase = []
                                , addedItemsThisPhase = []
                                , cmd = Cmd.none
                                }

                        GameOver time ->
                            { phase = GameOver (time + dt)
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

                nickIsAttacking =
                    enemies_ |> List.any (\enemy -> enemy.kind == Nick && enemy.isAttacking)

                wasKilled kind =
                    let
                        inFirstList =
                            List.any (\enemy -> enemy.kind == kind) world.enemies

                        inSecondList =
                            List.any (\enemy -> enemy.kind == kind) enemies
                    in
                    inFirstList && not inSecondList

                wasDamaged : EnemyKind -> Bool
                wasDamaged kind =
                    Maybe.map2 (\a b -> a.health < b.health)
                        (enemies |> List.filter (\e -> e.kind == kind) |> List.head)
                        (world.enemies |> List.filter (\e -> e.kind == kind) |> List.head)
                        |> Maybe.withDefault False

                justPickedUp : ItemKind -> Bool
                justPickedUp kind =
                    pickedUpItems |> List.any (.kind >> (==) kind)
            in
            ( { model
                | world =
                    { world
                        | player =
                            { player
                                | gems = List.foldr updateGems player.gems pickedUpItems
                                , hasSword = player.hasSword || (pickedUpItems |> List.any (.kind >> (==) Sword))
                                , hasArtifact = player.hasArtifact || justPickedUp Artifact
                                , health = health
                                , hasDash = player.hasDash || wasKilled Kelch
                                , hasFireball = player.hasFireball || wasKilled Nick
                            }
                        , items = remainingItems ++ addedItemsThisPhase
                        , enemies = enemies ++ addedEnemiesThisPhase
                    }
                , ticks = ticks
                , visited = visited
                , phase = phase
              }
            , Cmd.batch
                [ cmd
                , if wasDamaged Kelch then
                    Ports.kelchHit

                  else if wasDamaged Nick then
                    Ports.nickHit

                  else if wasKilled Kelch then
                    Ports.kelchKilled

                  else if kelchIsInView then
                    Ports.kelchTaunt

                  else if wasKilled Nick then
                    Ports.nickKilled

                  else if nickIsAttacking then
                    Ports.nickTaunt

                  else
                    Cmd.none
                ]
            )

        OpenShopMenu ->
            if player_.hasShield then
                ( model, Ports.dhruvPost )

            else
                ( { model | menu = ShopMenu }, Ports.dhruv )

        SnootyLadySpeak ->
            ( model, Ports.snootyLady )

        FanBoySpeak ->
            ( model, Ports.fanboy )

        BlacksmithSpeak ->
            ( model, Ports.incoherentBlacksmith )

        ScottSpeak ->
            if player_.hasDash && player_.hasFireball then
                if player_.hasGateKey then
                    ( model, Ports.scottPost )

                else
                    ( { model | world = { world | player = { player_ | hasGateKey = True } } }
                    , Ports.scottGiveKey
                    )

            else
                ( model, Ports.scottPre )

        CloseShopMenu ->
            ( { model | menu = None }, Cmd.none )

        BoughtShield ->
            ( { model | world = { world | player = { player_ | hasShield = True } }, menu = None }
            , Cmd.none
            )


poof =
    { frames = 5
    , duration = 600
    }


manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2))


withinDistance n player enemy =
    manhattanDistance ( player.x, player.y ) ( enemy.x, enemy.y ) < n * n * sizes.tile


updateEnemy : Float -> Model -> Enemy -> Enemy
updateEnemy dt { world, phase } enemy_ =
    let
        { player, terrain } =
            world

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
    if List.member enemy.kind [ RagingPig, Sandit ] && Maybe.andThen .target world.player.fireball == Just enemy then
        { enemy | health = 0 }

    else
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
                            attemptMovement phase sizes.enemy terrain enemy ( dx, dy )

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
updatePlayer dt ({ mouse, world } as model) =
    let
        { player } =
            world

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
            not isBlocking && world.player.attackTimer == 0 && isHoldingAbility && player.hasFireball

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
            not isBlocking && world.player.attackTimer == 0 && isHoldingAttack && player.hasSword

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
                attemptMovement model.phase sizes.player world.terrain player ( dx / 4, dy / 4 )

            else
                attemptMovement model.phase sizes.player world.terrain player ( dx, dy )

        attackTimer =
            max 0 (world.player.attackTimer - dt)

        fireball =
            world.player.fireball
                |> Maybe.map
                    (\f ->
                        { f
                            | duration = max 0 (f.duration - dt)
                            , x = f.vx * dt * fireballAttack.speed + f.x
                            , target = world.enemies |> List.filter (\e -> doSquaresCollide (enemyToSquare e) (projectileToSquare f)) |> List.head
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
    | VillageGem
    | ForestGem
    | DesertGem
    | VolcanoGem
    | Artifact


updateGems : Item -> GemInventory -> GemInventory
updateGems item gems =
    case item.kind of
        VillageGem ->
            { gems | village = gems.village + 1 }

        ForestGem ->
            { gems | forest = gems.forest + 1 }

        DesertGem ->
            { gems | desert = gems.desert + 1 }

        VolcanoGem ->
            { gems | volcano = gems.volcano + 1 }

        Sword ->
            gems

        Artifact ->
            gems


type alias Square =
    { x : Float, y : Float, size : Float }


npcToSquare : Npc -> Square
npcToSquare npc =
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
enemyToSquare e =
    { x = e.x
    , y = e.y
    , size = sizes.enemy
    }


playerToSquare : Player -> Square
playerToSquare player =
    { x = player.x
    , y = player.y
    , size = sizes.player
    }


nearbyNpc : World -> Maybe Npc
nearbyNpc world =
    world.npcs
        |> List.filter (\npc -> doSquaresCollide (npcToSquare npc) (playerToSquare world.player))
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

        FightingBoss _ ->
            { whiteGrass = False
            , rectangleWorld = False
            , blackAndWhiteHero = False
            , npcsEnabled = True
            , canPickupItems = True
            , canUseBridge = True
            , canTakeDamage = True
            , canViewGold = True
            }

        GameOver _ ->
            { whiteGrass = False
            , rectangleWorld = False
            , blackAndWhiteHero = False
            , npcsEnabled = True
            , canPickupItems = True
            , canUseBridge = True
            , canTakeDamage = True
            , canViewGold = True
            }


aboveGate : World -> Bool
aboveGate world =
    Dict.get (tileBelowPlayer world.player) world.terrain == Just Gate


tileBelowPlayer : Player -> ( Int, Int )
tileBelowPlayer player =
    ( round (player.x / sizes.tile), round (player.y / sizes.tile) + 1 )


view : Shared.Model -> Model -> View Msg
view shared ({ world } as model) =
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
                , centeredOn = ( world.player.x + sizes.player / 2, world.player.y + sizes.player / 2 )
                }
                (case ( model.spritesheet, Dict.size world.terrain ) of
                    ( _, 0 ) ->
                        []

                    ( Nothing, _ ) ->
                        []

                    ( Just spritesheet, _ ) ->
                        viewGame spritesheet model
                )
            , if world.player.hasGateKey && aboveGate world then
                Html.div [ Attr.class "dialogue-prompt" ]
                    [ Html.strong [] [ Html.text "Unlock" ]
                    , Html.text " gate with key."
                    ]

              else if canUnlockLowerGates world || canUnlockVolcanoGate world then
                Html.div [ Attr.class "dialogue-prompt" ]
                    [ Html.strong [] [ Html.text "Unlock" ]
                    , Html.text " gates with gems."
                    ]

              else
                case nearbyNpc world of
                    Just npc ->
                        if (phases model.phase).npcsEnabled then
                            Html.div [ Attr.class "dialogue-prompt" ]
                                [ Html.text "Talk to "
                                , Html.strong [] [ Html.text (nameOfNpc npc) ]
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

                        FightingBoss _ ->
                            []

                        GameOver _ ->
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
                            , if world.player.hasShield then
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
                    case model.phase of
                        GameOver _ ->
                            Html.div [ Attr.class "overlay" ]
                                [ Html.div [ Attr.class "pause" ]
                                    [ Html.h3 [ Attr.class "title" ] [ Html.text "Victory!" ]
                                    , Html.div [] [ Html.text "Thanks for playing Unblank!" ]
                                    ]
                                ]

                        _ ->
                            Html.text ""
            ]
        ]
    }


canUnlockLowerGates : World -> Bool
canUnlockLowerGates world =
    hasAllLowerGems world.player && belowOneOfTheseGates [ VillageGate, ForestGate, DesertGate ] world


canUnlockVolcanoGate : World -> Bool
canUnlockVolcanoGate world =
    world.player.gems.volcano == 5 && Dict.get (playerCoordinates world.player |> Tuple.mapFirst ((+) 1)) world.terrain == Just VolcanoGate


hasAllLowerGems : Player -> Bool
hasAllLowerGems player =
    List.all ((==) 5)
        [ player.gems.village
        , player.gems.forest
        , player.gems.desert
        ]


belowOneOfTheseGates : List Tile -> World -> Bool
belowOneOfTheseGates tiles world =
    let
        tile =
            Dict.get (playerCoordinates world.player) world.terrain
    in
    List.member tile (List.map Just tiles)


playerCoordinates : Player -> ( Int, Int )
playerCoordinates player =
    ( round (player.x / sizes.tile)
    , round (player.y / sizes.tile)
    )


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
viewGame spritesheet ({ world } as model) =
    let
        sprites =
            { sword = ( 0, 8 )
            , gems =
                { village = ( 6, 8 )
                , forest = ( 7, 8 )
                , desert = ( 8, 8 )
                , volcano = ( 9, 8 )
                }
            , artifact = ( 2, 8 )
            , tree = ( 4, 8 )
            , gate = ( 5, 7 )
            , villageGate = ( world.player.gems.village, 0 )
            , forestGate = ( world.player.gems.forest, 1 )
            , desertGate = ( world.player.gems.desert, 2 )
            , volcanoGate = ( world.player.gems.volcano, 3 )
            }

        viewItem item =
            let
                sprite =
                    case item.kind of
                        Sword ->
                            sprites.sword

                        VillageGem ->
                            sprites.gems.village

                        ForestGem ->
                            sprites.gems.forest

                        DesertGem ->
                            sprites.gems.desert

                        VolcanoGem ->
                            sprites.gems.volcano

                        Artifact ->
                            sprites.artifact
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

        select =
            Elm2D.Spritesheet.select spritesheet

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
                            { sprite = select sprites.tree
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

                    Gate ->
                        Elm2D.sprite
                            { size = size
                            , position = position
                            , sprite = select sprites.gate
                            }

                    VillageGate ->
                        Elm2D.sprite
                            { size = size
                            , position = position
                            , sprite = select sprites.villageGate
                            }

                    ForestGate ->
                        Elm2D.sprite
                            { size = size
                            , position = position
                            , sprite = select sprites.forestGate
                            }

                    DesertGate ->
                        Elm2D.sprite
                            { size = size
                            , position = position
                            , sprite = select sprites.desertGate
                            }

                    VolcanoGate ->
                        Elm2D.sprite
                            { size = size
                            , position = position
                            , sprite = select sprites.volcanoGate
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

                    Void ->
                        Elm2D.rectangle
                            { size = size
                            , position = position
                            , color = colorFromTuple colors.void
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
                    { left = floor ((world.player.x - camera.width / 2) / sizes.tile)
                    , top = floor ((world.player.y - camera.height / 2) / sizes.tile)
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
                    (\xy -> Dict.get xy world.terrain |> Maybe.map (viewWorldTile xy))

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
                    List.map viewItem world.items

                else
                    []
            , enemies = List.map (viewEnemy spritesheet model) world.enemies
            , npcs = List.map (viewNpc spritesheet model) world.npcs
            , player =
                [ ( world.player.y
                  , Elm2D.sprite
                        { sprite = viewPlayer spritesheet model
                        , size = ( sizes.player, sizes.player )
                        , position = ( world.player.x, world.player.y )
                        }
                  )
                ]
            , projectiles = List.filterMap (Maybe.map viewFireball) [ world.player.fireball ]
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


viewPlayer : Spritesheet -> Model -> Sprite
viewPlayer spritesheet ({ world } as model) =
    let
        itemOffset =
            if world.player.hasArtifact then
                6

            else if world.player.hasShield then
                4

            else if world.player.hasSword then
                2

            else
                0

        col =
            case world.player.direction of
                Right ->
                    8 + itemOffset

                Left ->
                    9 + itemOffset
    in
    case world.player.animation of
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
viewEnemy spritesheet ({ world } as model) ({ x, y } as enemy) =
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

                Nick ->
                    27

                Null ->
                    26

        size =
            ( sizes.enemy, sizes.enemy )

        position =
            case enemy.animation of
                Attacking ms _ ->
                    ( (cos (ms / durations.enemyAttack / 2) * (world.player.x - x) / 4) + x
                    , (cos (ms / durations.enemyAttack / 2) * (world.player.y - y) / 4) + y
                    )

                _ ->
                    ( x, y )

        enemySprite =
            case enemy.animation of
                Idle ->
                    Elm2D.Spritesheet.select spritesheet ( col, row )

                Running ->
                    case enemy.kind of
                        Null ->
                            Elm2D.Spritesheet.select spritesheet ( col, row )

                        Nick ->
                            Elm2D.Spritesheet.frame (modBy 2 (round model.ticks // 150))
                                (Elm2D.Spritesheet.animation spritesheet [ ( col, row + 1 ), ( col, row + 2 ) ])

                        _ ->
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
        , ( "Scott", ( 3 + col, 16 ) )
        , ( "lady", ( 3, 11 ) )
        , ( "boy", ( 4, 11 ) )
        , ( "Blacksmith", ( 3, 12 ) )
        ]


viewNpc : Spritesheet -> Model -> Npc -> ( Float, Elm2D.Element )
viewNpc spritesheet model npc =
    let
        col =
            case npc.direction of
                Right ->
                    0

                Left ->
                    1

        sprite =
            Dict.get (nameOfNpc npc) (npcSpriteMapping col)
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


viewGem : String -> Int -> Html msg
viewGem modifierClass count =
    Html.div [ Attr.class "gem", Attr.classList [ ( modifierClass, True ), ( "gem--visible", count > 0 ) ] ] [ Html.text (count |> String.fromInt) ]


viewHud : Model -> Html Msg
viewHud ({ world } as model) =
    Html.div [ Attr.class "hud" ]
        [ Html.div [ Attr.class "gems" ]
            [ viewGem "gem--village" world.player.gems.village
            , viewGem "gem--forest" world.player.gems.forest
            , viewGem "gem--desert" world.player.gems.desert
            , viewGem "gem--volcano" world.player.gems.volcano
            ]

        -- , if (phases model.phase).canTakeDamage then
        --     Html.progress
        --         [ Attr.class "healthbar"
        --         , Attr.classList [ ("healthbar--low", world.player.health > 0 && world.player.health < 0.3 * world.player.maxHealth )]
        --         , Attr.value
        --             (if world.player.health == world.player.maxHealth then
        --                 "1"
        --              else
        --                 (toFloat world.player.health / toFloat world.player.maxHealth) |> String.fromFloat
        --             )
        --         ]
        --         []
        --   else
        --     Html.text ""
        -- ]
        ]
