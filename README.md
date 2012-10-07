Ninjas
======

Ninjas is a multiplayer game where players attempt to blend in with the
computer controlled characters on their quest to either loot all five
castles or to eliminate all of the other players.

NPCs will move randomly, will not die, and will not attack.

Players can stun NPCs, kill other players, attempt to regain anonymity using
the smoke bomb, and attempt to secretly loot the castles.

When any player loots a castle he hasn't looted before the text "DING" will
appear in the top corner of the screen. No further indication is given as to
which castle was looted or which character was the looter.

Controls
--------

* **A** - Attack
* **S** - Smoke bomb
* **N** - New game
* **Left Click** - Move to clicked location
* **Right Click** - Stop moving immediately

Installation
------------

    $ cabal install

Usage
-----

    Ninjas server [FLAGS] NUM_NINJAS
        --port=NUM    Server port
        --npcs=NUM    Number of NPCs
        --smokes=NUM  Number of initial smokebombs
    
    Ninjas client [FLAGS] [HOSTNAME [PORT]]
        --server=STRING  Server hostname
        --port=NUM       Server port
        --user=STRING    User Name
