{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Data.Maybe (catMaybes)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random (randomRIO)

import NetworkMessages

boardMin :: Point
boardMin = (-250,-250)

boardMax :: Point
boardMax = (250,250)

speed :: Float
speed = 1

restTime :: Float
restTime = 2

ninjaRadius :: Float
ninjaRadius = 10

attackDistance :: Float
attackDistance = 100

attackAngle    :: Float
attackAngle    = pi / 4

stunTime :: Float
stunTime = 3

eventsPerSecond :: Int
eventsPerSecond = 100


data NPC      = NPC
  { npcName   :: Int
  , npcPos    :: Point
  , npcFacing :: Vector -- Unit vector
  , npcState  :: State
  }
  deriving (Read, Show, Eq)

data Player   = Player { playerNpc :: NPC }
  deriving (Show, Read, Eq)

data State    = Walking WalkInfo | Waiting WaitInfo | Dead
  deriving (Show, Read, Eq)

data WalkInfo = Walk { npcTarget    :: Point
                     -- Cached, so that we don't recompute all the time.
                     , npcDist      :: Float
                     , npcVelocity  :: Vector
                     }
  deriving (Show, Read, Eq)

data WaitInfo = Wait { npcWaiting :: Maybe Float, npcStunned :: Bool }
  deriving (Show, Read, Eq)

data World = World
  { worldNpcs        :: [NPC]
  }

data ServerWorld = ServerWorld
  { serverNpcs    :: [NPC]
  , serverPlayers :: [Player]
  }

data ThinkTask = ChooseWait | ChooseDestination

{- This performs one time tick update of a character.  It is important
that we perofrm this operation atomically to avoid a race conditions
with external threads that try to modify the state (e.g., user input
or commands from the server).  In addition to returning an updated NPC,
we also return a kind of "continutaion" telling us what needs to be
done next for computer controlled characters. -}
updateNPC' :: Float -> NPC -> (NPC, Maybe ThinkTask)
updateNPC' elapsed npc =
  case state of
    Walking w
      | npcDist w < speed -> done ChooseWait
      | otherwise -> working (Walking w { npcDist = npcDist w - speed })
                             npc { npcPos = addPt (npcVelocity w) (npcPos npc) }

    Waiting w ->
      case npcWaiting w of
        Nothing -> working state npc
        Just todo
          | todo < elapsed -> done ChooseDestination
          | otherwise ->
              working (Waiting w { npcWaiting = Just (todo - elapsed) }) npc

    Dead -> working state npc

  where state = npcState npc

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


performAttack :: Player -> [Player] -> [NPC] -> (Player, [Player], [NPC], [ServerCommand])
performAttack attacker players npcs =
  (attacker , players' , npcs' , attackCmd : catMaybes (commands1 ++ commands2))
  where
  attackCmd = ServerCommand (npcName pnpc) Attack

  (players', commands1) = unzip $ map checkKill players
  (npcs'   , commands2) = unzip $ map checkStun npcs

  pnpc             = playerNpc attacker

  affected npc     = attackLen       <= attackDistance
                  && cos attackAngle <= npcFacing pnpc `dotV` attackVector1
    where
    attackVector   = subPt (npcPos npc) (npcPos pnpc)
    attackLen      = magV attackVector
    attackVector1  = mulSV (recip attackLen) attackVector

  checkKill player
    | affected npc = ( player { playerNpc = deadNPC npc }
                     , Just (ServerCommand (npcName npc) Die)
                     )
    | otherwise    = ( player, Nothing )
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

initPlayer :: Int -> IO Player
initPlayer name =
  do playerNpc <- initServerNPC False name
     return Player { .. }

pickWaitTime :: Bool -> IO (Maybe Float)
pickWaitTime False = return Nothing
pickWaitTime True  = fmap Just $ randomRIO (0, restTime)
