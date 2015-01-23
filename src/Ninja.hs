-- |The Ninja module is designed to help you build bots
-- that play the 'Ninjas' game.  Bots must be a module with
-- function @ninja :: Ninja@ and a stance @ninjaStance :: Stance@.
--
-- There are several levels of ninja, divided into belts of white, blue,
-- green, red, and black.
--
-- The simplest ninja is a 'whiteBelt'. White belts can
-- control when to attack and where to move while considering
-- conditional statements reguarding the entire board.  For example:
--
-- @
-- -- FIXME whitebelt example
-- @
--
-- Upon advancement to the blue belt, ninjas obtain better focus.  Blue
-- belts begin to consider their adversaries individually, deciding to
-- follow or avoid each enemy in turn.
--
-- @
-- -- FIXME bluebelt example
-- @
--
-- Unfortunately, blue belts tend to forget the patterns of their enemies
-- but with training they will advance to the green belt.  At that time,
-- they should learn to remember aspects of their targets behavior and
-- begin to recognize patterns.
--
-- @
-- -- FIXME greenbelt example
-- @
--
-- Green belts are powerful opponents, but their inability to distinguish
-- friend from foe makes them dangerous to themselves.  To obtain a red
-- belt they must be able to distinguish between threats and bystanders.
--
-- @
-- -- FIXME redbelt example
-- @
--
-- To become a ninja there must be no aspect of the battle that is
-- omitted.  All sights, sounds, and knowledge must be used to defeat
-- your enemies.
module Ninja
    (
    -- * Types
    -- ** Basic
    Ninja, Stance,
    -- ** Advanced
    -- *** Triggers
    BotEvent(..), ServerCommand(..), Command (..), BotHandle,
    -- *** World data
    InfoWorld(..),
    ClientCharacter(..), Character(..), State(..),
    WalkInfo(..), WaitInfo(..),
    -- * World Queries
    boardMax, boardMin,
    -- * Primitive Commands
    move, stop, attack, smoke,
    -- * Administrative
    start
    ) where

import Parameters
import Data.Dynamic
import NetworkMessages (ServerCommand(..), Command(..))
import Bot

type Stance = Dynamic
type Ninja  = BotHandle -> BotEvent -> Dynamic -> IO Dynamic
