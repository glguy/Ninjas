{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Data.Maybe (catMaybes)
import Data.List  (findIndex)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random (randomRIO)

import NetworkMessages

-- | Smallest point on the board
boardMin :: Point
boardMin = (-350,-250)

-- | Largest point on the board
boardMax :: Point
boardMax = (350,250)

-- | Pixels per second traveled by ninjas
speed :: Float
speed = 100

-- | Duration in seconds a player is stunned after an attack
attackDelay :: Float
attackDelay = 1

-- | Maximum time an NPC will wait before choosing a new destination
restTime :: Float
restTime = 2

-- | Radius of ninja sprite in pixels. This is used for drawing
-- a ninja and determining when he is hidden under smoke.
ninjaRadius :: Float
ninjaRadius = 10

-- | Maximum distance from the center of a player where an attack has an effect
attackDistance :: Float
attackDistance = 75

-- | Maximum angle in radians in either direction from a player's direction of
-- travel where the attacks have an effect
attackAngle    :: Float
attackAngle    = pi / 4

-- | Duration in second for which an NPC will be stunned after an attack
stunTime :: Float
stunTime = 3

-- | The number of time update events gloss should attempt to run per second
eventsPerSecond :: Int
eventsPerSecond = 100

-- | The locations of the centers of the win locations in the game
pillars :: [Point]
pillars = [(0,0), (275, 175), (-275, 175), (-275, -175), (275, -175)]

-- | The length of a side of the win location squares
pillarSize :: Float
pillarSize = 30

frameWait :: Float
frameWait = 0.2

data NPC      = NPC
  { npcName   :: Int
  , npcPos    :: Point
  , npcFacing :: Vector -- Unit vector
  , npcState  :: State
  }
  deriving (Read, Show, Eq)

data Player   = Player
  { playerNpc      :: NPC
  , playerUsername :: String
  , playerScore    :: Int
  , playerVisited  :: [Int]
  , playerSmokes   :: Int
  }
  deriving (Show, Read, Eq)

data State
  = Walking WalkInfo
  | Waiting WaitInfo
  | Dead
  | Attacking Float
  deriving (Show, Read, Eq)

data WalkInfo = Walk { npcTarget    :: Point
                     , npcWalkFrame :: (Float, Int)
                     -- Cached, so that we don't recompute all the time.
                     , npcDist      :: Float
                     , npcVelocity  :: Vector
                     }
  deriving (Show, Read, Eq)

data WaitInfo = Wait { npcWaiting :: Maybe Float, npcStunned :: Bool }
  deriving (Show, Read, Eq)

data World = World
  { worldNpcs        :: [NPC]
  , dingTimers       :: [Float]
  , smokeTimers      :: [(Float, Point)]
  , worldMessages    :: [String]
  }

data ServerWorld = ServerWorld
  { serverNpcs    :: [NPC]
  , serverPlayers :: [Player]
  , serverMode   :: ServerMode
  }

data ServerMode = Playing | Starting | Stopped
  deriving (Eq, Read, Show)

data ThinkTask = ChooseWait | ChooseDestination

-- | Compute a new NPC given a number of elapsed seconds. An optional
-- update task will be returned if the server should compute a new
-- goal for the NPC. Clients will ignore this task and wait for the
-- server to send an update.
updateNPC' ::
  Float {- ^ elapsed seconds -} ->
  NPC   ->
  (NPC, Maybe ThinkTask)
updateNPC' elapsed npc =
  case state of
    Walking w
      | npcDist w < step -> done ChooseWait
      | otherwise -> working (Walking w { npcDist = npcDist w - step
                                        , npcWalkFrame = nextFrame })
                             npc { npcPos = addPt (mulSV elapsed (npcVelocity w)) (npcPos npc) }

      where step = elapsed * speed
            nextFrame = case npcWalkFrame w of
                          (x,n) | x < elapsed -> (frameWait, (n + 1) `mod` 4)
                                | otherwise   -> (x - elapsed, n)
    Waiting w ->
      case npcWaiting w of
        Nothing -> working state npc
        Just todo
          | todo < elapsed -> done ChooseDestination
          | otherwise ->
              working (Waiting w { npcWaiting = Just (todo - elapsed) }) npc


    Attacking delay
      | elapsed > delay -> (waitingNPC npc Nothing False                  , Nothing)
      | otherwise       -> (npc { npcState = Attacking (delay - elapsed)} , Nothing)

    Dead -> (npc, Nothing)

  where 

        state = npcState npc

        done next = (npc { npcState = Waiting Wait { npcWaiting = Nothing, npcStunned = False } }
                    , Just next)

        working s n = (n { npcState = s}, Nothing)

-- | Lift a function on NPCs to one on Players
mapPlayerNpc :: (NPC -> NPC) -> Player -> Player
mapPlayerNpc f p = p { playerNpc = f (playerNpc p) }

-- | Update an NPC to have the goal of walking to a given point.
walkingNPC :: NPC -> Point -> NPC
walkingNPC npc npcTarget = npc { npcState = state
                               , npcFacing = facing }
  where
  state       = Walking Walk { .. }

  facing      | npcDist > 0.001 = mulSV (1 / npcDist) path
              | otherwise       = npcFacing npc

  npcVelocity = mulSV speed facing
  npcWalkFrame = case npcState npc of
                   Walking Walk { npcWalkFrame = x } -> x
                   _ -> (frameWait, 0)

  path        = subPt npcTarget (npcPos npc)
  npcDist     = magV path

-- | Update an NPC to have the goal of waiting for an optional
-- duration and optionally to be drawn as stunned. If a duration
-- is specified the NPC will be updated automatically by the server.
-- NPCs without a duration are typically players.
waitingNPC :: NPC -> Maybe Float -> Bool -> NPC
waitingNPC npc npcWaiting npcStunned = npc { npcState = state }
  where
  state = Waiting Wait { .. }

-- | Update an NPC to be dead
deadNPC :: NPC -> NPC
deadNPC npc = npc { npcState = Dead }

-- | Update an NPC to be stunned for the default stun duration.
stunnedNPC :: NPC -> NPC
stunnedNPC npc = waitingNPC npc (Just stunTime) True

-- | Update an NPC to be in the attacking state for the
-- default attack duration.
attackNPC :: NPC -> NPC
attackNPC npc = npc { npcState = Attacking attackDelay }

-- | Given an attacking player, the other players, and the NPCs
-- compute the new state of the attacker, the other players, and
-- the NPCs as well as a list of messages to broadcast to all
-- players and a list of the names of players who were killed
-- in the attack.
performAttack ::
  Player   {- ^ attacker              -} ->
  [Player] {- ^ possible kill targets -} ->
  [NPC]    {- ^ possible stun targets -} ->
  (Player, [Player], [NPC], [ServerCommand], [Int])
performAttack attacker players npcs =
  ( mapPlayerNpc attackNPC attacker
  , players'
  , npcs'
  , attackCmd : catMaybes (commands1 ++ commands2)
  , catMaybes deathnotes
  )
  where
  attackCmd = ServerCommand (npcName pnpc) Attack

  (players', commands1, deathnotes) = unzip3 $ map checkKill players
  (npcs'   , commands2) = unzip $ map checkStun npcs

  pnpc             = playerNpc attacker

  affected npc     = attackLen       <= attackDistance
                  && cos attackAngle <= npcFacing pnpc `dotV` attackVector1
    where
    attackVector   = subPt (npcPos npc) (npcPos pnpc)
    attackLen      = magV attackVector
    attackVector1  = mulSV (recip attackLen) attackVector

  checkKill player
    | npcState npc /= Dead && affected npc
          = ( player { playerNpc = deadNPC npc }
                     , Just (ServerCommand (npcName npc) Die)
                     , Just (npcName npc)
                     )
    | otherwise    = ( player, Nothing, Nothing )
    where
    npc = playerNpc player

  checkStun npc
    | not (isStunned npc) && affected npc
                   = ( stunnedNPC npc
                     , Just (ServerCommand (npcName npc) Stun)
                     )
    | otherwise    = ( npc, Nothing )

isStunned :: NPC -> Bool
isStunned npc =
  case npcState npc of
    Waiting Wait { npcStunned = True } -> True
    _                                  -> False

-- | Compute a random point inside a box.
randomPoint :: Point -> Point -> IO Point
randomPoint (minX,minY) (maxX,maxY) =
  do x <- randomRIO (minX,maxX)
     y <- randomRIO (minY,maxY)
     return (x,y)

-- | Compute a random point inside the board.
randomBoardPoint :: IO Point
randomBoardPoint = randomPoint boardMin boardMax

-- | Compute a random unit vector.
randomUnitVector :: IO Vector
randomUnitVector =
  do degrees <- randomRIO (0,359)
     let rads = degToRad $ fromInteger degrees
     return $ unitVectorAtAngle rads

-- | Add two vectors
addPt :: Vector -> Vector -> Vector
addPt (x,y) (a,b) = (x+a,y+b)

-- | Subtract two vectors
subPt :: Vector -> Vector -> Vector
subPt (x,y) (a,b) = (x-a,y-b)

-- | Construct a new NPC given a name, a position,
-- and a facing unit vector. This function is used
-- by clients who are told the parameters by the
-- server.
initClientNPC :: Int -> (Point, Vector) -> NPC
initClientNPC npcName (npcPos, npcFacing) =
  let npcState = Waiting Wait { npcWaiting = Nothing, npcStunned = False }
  in  NPC { .. }

-- | Construct a new NPC given a name, a position,
-- and a facing unit vector. When think is True,
-- the NPC will be scheduled to begin walking
-- after a random duration.
initServerNPC :: Bool -> Int -> IO NPC
initServerNPC think npcName =
  do npcPos     <- randomBoardPoint
     npcFacing  <- randomUnitVector
     npcWaiting <- pickWaitTime think
     let npcStunned = False
         npcState = Waiting Wait { .. }
     return NPC { .. }

-- | Construct a new player given an initial number
-- of smokebombs, an identifier, a username, and a
-- starting score.
initPlayer :: Int -> Int -> (String,Int) -> IO Player
initPlayer smokes name (playerUsername,playerScore) =
  do playerNpc <- initServerNPC False name
     let playerVisited = []
         playerSmokes  = smokes
     return Player { .. }

-- | When True, compute a new random wait time value.
pickWaitTime ::
  Bool {- ^ compute random delay -} ->
  IO (Maybe Float)
pickWaitTime False = return Nothing
pickWaitTime True  = fmap Just $ randomRIO (0, restTime)

-- | Determine if a point lies within a given pillar.
isInPillar ::
  Point {- ^ location to test -} ->
  Point {- ^ center of pillar -} ->
  Bool
isInPillar p (x,y) = pointInBox p (x-pillarSize/2,y-pillarSize/2) (x+pillarSize/2, y+pillarSize/2)

-- | Determine the index, if any, of the pillar with contains a point.
whichPillar :: Point -> Maybe Int
whichPillar p = findIndex (isInPillar p) pillars

-- | Return true if the given player can use smokebombs.
hasSmokebombs :: Player -> Bool
hasSmokebombs p = playerSmokes p > 0

-- | Update a player to have one fewer smokebombs.
consumeSmokebomb :: Player -> Player
consumeSmokebomb p = p { playerSmokes = playerSmokes p - 1 }
