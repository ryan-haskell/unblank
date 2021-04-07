# Progression

## Phase 1

Message: "WASD to move"

The player character exists on a blank world. Non-walkable areas are black, walkable areas are
white. After moving X distance, a sword spawns in on a nearby walkable tile. The player picks
up the sword.

Code Work:

*   Basic movement [Done]
*   Item pickup [Done]
*   Notification messages [Done]
*   "Fog of War" [Done]

## Phase 2

Message: "Left Click to attack"

After the player swings the sword, some minor enemies begin to spawn on nearby walkable tiles.
Hitting an enemy with the sword causes damage. Killing an enemy causes color to fade in
to the world. Killing the fifth enemy brings the world to full color.

Code Work:

*   Melee combat
    *   Attack animation [Done]
    *   Attack sound
    *   Enemy damage
    *   Enemy death
*   Enemy spawning
*   Enemy type A
    *   Sprites [Done]
    *   AI
*   Backgound color fade in

## Phase 3

With the world now in full color, some background music begins to play. Defeating enemies now
causes them to drop items (gold, health). [Alt: health bar and heal items don't appear until
player takes damage for first time?]. Picking up the first gold causes a shopkeep to appear.
Talking to the shopkeep opens a shopping menu, they are selling a shield for 5 gold. The
player buys the shield.

Code Work:

*   Item dropping on enemy death
*   Player health
*   Player taking damage
    *   Animation
    *   Sound
*   Game over / player respawn
*   Shopkeeper NPC [Done]
*   Shop menu

## Phase 4

Message: "Right click to block"

A new enemy type spawns in that warrents blocking. With each successful block, a bridge begins
to fade into existance over the nearby river. Crossing the bridge leads the player to a village.

Code work:

*   Enemy type B
*   Shield combat
    *   Animation / Sprites [Done]
    *   Blocking enemy attacks
    *   Sound
*   Bridge fade in

## Phase 5

The player walks into a small village. The village is empty except for a man wearing a crown in
the village center. Talking to him gives dialog (something something, save the world, talk to
villagers for help). The player gets a quest to retrieve (3?) magic things. Other villagers spawn
into existance around the town. One villager is a shopkeeper with (cosmetics?, buffs?). One
villager has a quest to kill 3 of enemy B. Completing this quest rewards dodge roll. Acquiring
the dodge roll causes a stmina meter to appear.

Code Work:

*   NPC dialog
*   Quest pop-up
*   Quest menu (tab?)
*   Quest tracking
*   Quest completion
*   Dodge roll / dash
    *   Stamina meter
    *   Animation / sprites
    *   Implementation
    *   Sound Effect
*   Cosmetics
*   Player buffs / upgrades

## Phase 6

West of village there's a bandit camp. In center there's a chest with strong attack. East of village
there's a bandit camp. In center there's a chest with fireball. Various buff / upgrade items can be
found on the map. The strong attack uses stamina. Acquiring the fireball causes a mana meter to appear,
and enemies can now drop mana potions.

Code work:

*   More enemy types
*   Treasure chests
*   Strong attack
    *   Sprite & Animation
    *   Sound
*   Fireball
    *   Sprite & Animation
    *   Sound
*   Obstacle breakable by strong attack
*   Obstacle breakable by fireball

## Phase 7

North of the village is a third bandit camp. This camp contains a chest with (void undestruction?).
With this the player can proceed east to the void and finish the game with a maybe boss fight?

Code work: 

*   More enemy types
*   Void Undestruction
*   Boss Fight?
*   The End Screen & Credits?
