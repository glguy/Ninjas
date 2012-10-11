{-# LANGUAGE RecordWildCards #-}
module Character where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

import VectorUtils
import Parameters

data Character = Character
  { charName   :: Int
  , charPos    :: Point
  , charFacing :: Vector -- Unit vector
  , charState  :: State
  }
  deriving (Read, Show, Eq)

data WalkInfo = Walk
  { walkTarget    :: Point
  -- Cached, so that we don't recompute all the time.
  , walkDist      :: Float
  , walkVelocity  :: Vector
  }
  deriving (Show, Read, Eq)

data WaitInfo = Wait
  { waitWaiting :: Maybe Float
  , waitStunned :: Bool
  }
  deriving (Show, Read, Eq)

data State
  = Walking WalkInfo
  | Waiting WaitInfo
  | Dead
  | Attacking Float
  deriving (Show, Read, Eq)

data ThinkTask = ChooseWait | ChooseDestination

-- | Compute a new character given a number of elapsed seconds. An optional
-- update task will be returned if the server should compute a new
-- goal for the character. Clients will ignore this task and wait for the
-- server to send an update.
stepCharacter ::
  Float {- ^ elapsed seconds -} ->
  Character ->
  (Character, Bool, Maybe ThinkTask) -- (new character, did we change states, AI task)
stepCharacter elapsed c =
  case state of
    Walking w
      | walkDist w < step -> done (Just ChooseWait)
      | otherwise ->
        working (Walking w { walkDist = walkDist w - step })
                c { charPos = newPos }

      where step = elapsed * speed
            newPos = addPt (mulSV elapsed (walkVelocity w)) (charPos c)

    Waiting w ->
      case waitWaiting w of
        Nothing -> working state c
        Just todo
          | todo < elapsed -> done (Just ChooseDestination)
          | otherwise ->
              working (Waiting w { waitWaiting = Just (todo - elapsed) }) c


    Attacking delay
      | elapsed > delay -> done Nothing
      | otherwise       -> working (Attacking (delay - elapsed)) c

    Dead -> working state c

  where
  state       = charState c
  done next   = (c { charState = Waiting Wait { waitWaiting = Nothing
                                              , waitStunned = False } }
                , True
                , next)
  working s n = (n { charState = s}, False, Nothing)

-- | Update a Character to have the goal of walking to a given point.
walkingCharacter :: Point -> Character -> Character
walkingCharacter walkTarget c = c { charState = state
                                  , charFacing = facing }
  where
  state       = Walking Walk { .. }

  facing      | walkDist > 0.001 = mulSV (1 / walkDist) path
              | otherwise        = charFacing c

  walkVelocity = mulSV speed facing

  path        = subPt walkTarget (charPos c)
  walkDist    = magV path

-- | Update an character to have the goal of waiting for an optional
-- duration and optionally to be drawn as stunned. If a duration
-- is specified the character will be updated automatically by the server.
-- characters without a duration are typically players.
waitingCharacter :: Maybe Float -> Bool -> Character -> Character
waitingCharacter waitWaiting waitStunned c =
  c { charState = Waiting Wait { .. } }

-- | Update an character to be dead
deadCharacter :: Character -> Character
deadCharacter c = c { charState = Dead }

-- | Update an character to be stunned for the default stun duration.
stunnedCharacter :: Character -> Character
stunnedCharacter = waitingCharacter (Just stunTime) True

-- | Update an character to be in the attacking state for the
-- default attack duration.
attackingCharacter :: Character -> Character
attackingCharacter c = c { charState = Attacking attackDelay }

isStunned :: Character -> Bool
isStunned c =
  case charState c of
    Waiting Wait { waitStunned = True } -> True
    _                                   -> False
