{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Data.Maybe (catMaybes)
import Data.List  (findIndex)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random (randomRIO)

import NetworkMessages

boardMin :: Point
boardMin = (-350,-250)

boardMax :: Point
boardMax = (350,250)

speed :: Float
speed = 100

attackDelay :: Float
attackDelay = 1

restTime :: Float
restTime = 2

ninjaRadius :: Float
ninjaRadius = 10

attackDistance :: Float
attackDistance = 75

attackAngle    :: Float
attackAngle    = pi / 4

stunTime :: Float
stunTime = 3

eventsPerSecond :: Int
eventsPerSecond = 100

pillars :: [Point]
pillars = [(0,0), (275, 175), (-275, 175), (-275, -175), (275, -175)]

pillarSize :: Float
pillarSize = 30

frameWait :: Float
frameWait = 0.35

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

updateNPC' :: Float -> NPC -> (NPC, Maybe ThinkTask)
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

mapPlayerNpc :: (NPC -> NPC) -> Player -> Player
mapPlayerNpc f p = p { playerNpc = f (playerNpc p) }

insertPlayer :: Player -> [Player] -> [Player]
insertPlayer p [] = [p]
insertPlayer p (x:xs)
  | npcName (playerNpc p) < npcName (playerNpc x) = p : x : xs
  | otherwise = x : insertPlayer p xs

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

waitingNPC :: NPC -> Maybe Float -> Bool -> NPC
waitingNPC npc npcWaiting npcStunned = npc { npcState = state }
  where
  state = Waiting Wait { .. }

deadNPC :: NPC -> NPC
deadNPC npc = npc { npcState = Dead }

stunnedNPC :: NPC -> NPC
stunnedNPC npc = waitingNPC npc (Just stunTime) True

attackNPC :: NPC -> NPC
attackNPC npc = npc { npcState = Attacking attackDelay }

performAttack ::
  Player -> [Player] -> [NPC] ->
  (Player, [Player], [NPC], [ServerCommand], [(Int, String)])
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
                     , Just (npcName npc, playerUsername attacker)
                     )
    | otherwise    = ( player, Nothing, Nothing )
    where
    npc = playerNpc player

  checkStun npc
    | affected npc = ( stunnedNPC npc
                     , Just (ServerCommand (npcName npc) Stun)
                     )
    | otherwise    = ( npc, Nothing )


randomPoint :: Point -> Point -> IO Point
randomPoint (minX,minY) (maxX,maxY) =
  do x <- randomRIO (minX,maxX)
     y <- randomRIO (minY,maxY)
     return (x,y)

randomBoardPoint :: IO Point
randomBoardPoint = randomPoint boardMin boardMax

randomUnitVector :: IO Vector
randomUnitVector =
  do degrees <- randomRIO (0,359)
     let rads = degToRad $ fromInteger degrees
     return $ unitVectorAtAngle rads

addPt :: Point -> Point -> Point
addPt (x,y) (a,b) = (x+a,y+b)

subPt :: Point -> Point -> Point
subPt (x,y) (a,b) = (x-a,y-b)

initClientNPC :: Int -> (Point, Vector) -> NPC
initClientNPC npcName (npcPos, npcFacing) =
  let npcState = Waiting Wait { npcWaiting = Nothing, npcStunned = False }
  in  NPC { .. }

initServerNPC :: Bool -> Int -> IO NPC
initServerNPC think npcName =
  do npcPos     <- randomBoardPoint
     npcFacing  <- randomUnitVector
     npcWaiting <- pickWaitTime think
     let npcStunned = False
         npcState = Waiting Wait { .. }
     return NPC { .. }

initPlayer :: Int -> Int -> (String,Int) -> IO Player
initPlayer smokes name (playerUsername,playerScore) =
  do playerNpc <- initServerNPC False name
     let playerVisited = []
         playerSmokes  = smokes
     return Player { .. }

pickWaitTime :: Bool -> IO (Maybe Float)
pickWaitTime False = return Nothing
pickWaitTime True  = fmap Just $ randomRIO (0, restTime)

isInPillar :: Point -> Point -> Bool
isInPillar p (x,y) = pointInBox p (x-pillarSize/2,y-pillarSize/2) (x+pillarSize/2, y+pillarSize/2)

whichPillar :: Point -> Maybe Int
whichPillar p = findIndex (isInPillar p) pillars

hasSmokebombs :: Player -> Bool
hasSmokebombs p = playerSmokes p > 0

consumeSmokebomb :: Player -> Player
consumeSmokebomb p = p { playerSmokes = playerSmokes p - 1 }
