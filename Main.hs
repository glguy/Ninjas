{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Control.Monad
import Data.IORef


boardMin :: Point
boardMin = (-250,-250)

boardMax :: Point
boardMax = (250,250)

speed :: Float
speed = 1

restTime :: Float
restTime = 2

moveButton = MouseButton LeftButton
stopButton = MouseButton RightButton

main :: IO ()
main =
  do i <- initWorld
     playIO
       (InWindow "test" (round width, round height) (10,10)) black 100
       i
       drawWorld
       inputEvent
       updateWorld

  where (width,height) = subPt boardMax boardMin


addPt :: Point -> Point -> Point
addPt (x,y) (a,b) = (x+a,y+b)

subPt :: Point -> Point -> Point
subPt (x,y) (a,b) = (x-a,y-b)

len :: Point -> Float
len (x,y) = sqrt (x * x + y * y)

scalePt :: Float -> Point -> Point
scalePt s (x,y) = (s * x, s * y)


data NPC      = NPC { npcName :: Int, npcPos :: Point, npcState :: IORef State }

data Player   = Player { playerNpc :: NPC }

data State    = Walking WalkInfo | Waiting WaitInfo | Dead
  deriving (Show)

data WalkInfo = Walk { npcTarget    :: Point
                     -- Cached, so that we don't recompute all the time.
                     , npcDist      :: Float
                     , npcVelocity  :: Vector
                     }
  deriving (Show)

data WaitInfo = Wait { npcWaiting :: Maybe Float, npcStunned :: Bool }
  deriving (Show)

data World = World
  { worldNpcs :: [NPC]
  , worldPlayer :: Player
  , worldNpcChange :: NPC -> IO ()
  }

walkingNPC :: NPC -> Point -> IO ()
walkingNPC npc npcTarget = writeIORef (npcState npc) state
  where
  state       = Walking Walk { .. }
  npcVelocity | npcDist > 0.001 = scalePt (speed / npcDist) path
              | otherwise       = (0,0)

  path        = subPt npcTarget (npcPos npc)
  npcDist     = len path

waitingNPC :: NPC -> Maybe Float -> Bool -> IO ()
waitingNPC npc npcWaiting npcStunned = writeIORef (npcState npc) state
  where
  state = Waiting Wait { .. }


randomPoint :: Point -> Point -> IO Point
randomPoint (minX,minY) (maxX,maxY) =
  do x <- randomRIO (minX,maxX)
     y <- randomRIO (minY,maxY)
     return (x,y)

randomBoardPoint :: IO Point
randomBoardPoint = randomPoint boardMin boardMax

initNPC :: Bool -> Int -> IO NPC
initNPC think npcName =
  do npcPos   <- randomBoardPoint
     waitTime <- pickWaitTime think
     npcState <- newIORef (Waiting Wait { npcWaiting = waitTime, npcStunned = False })
     return NPC { .. }

initPlayer :: Int -> IO Player
initPlayer name =
  do pnpc <- initNPC False name
     return Player { playerNpc = pnpc }

pickWaitTime :: Bool -> IO (Maybe Float)
pickWaitTime False = return Nothing
pickWaitTime True  = fmap Just $ randomRIO (0, restTime)

updateNPC :: Float -> Bool -> NPC -> IO NPC
updateNPC t think npc = readIORef (npcState npc) >>= \state ->
  case state of
    Walking w
      | npcDist w < speed ->
          do time <- pickWaitTime think
             waitingNPC npc time False
             return npc

      | otherwise ->
          do writeIORef (npcState npc) (Walking w { npcDist = npcDist w - speed })
             return npc { npcPos = addPt (npcVelocity w) (npcPos npc) }
    Waiting w ->
      case npcWaiting w of
        Nothing -> return npc
        Just todo
          | todo < t  ->
              do tgt <- randomBoardPoint
                 walkingNPC npc tgt
                 announce npc
                 return npc

          | otherwise ->
              do writeIORef (npcState npc) (Waiting w { npcWaiting = Just $ todo - t })
                 return npc

    Dead -> return npc



initWorld      :: IO World
initWorld       = do p    <- initPlayer 0
                     npcs <- mapM (initNPC True) [1..25]
                     return World { worldPlayer    = p
                                  , worldNpcChange = announce
                                  , worldNpcs      = npcs }

announce :: NPC -> IO ()
announce npc =
  do state <- readIORef (npcState npc)
     print (npcName npc, state)

drawWorld      :: World -> IO Picture
drawWorld w     = fmap pictures $ mapM drawNPC $ playerNpc (worldPlayer w) : worldNpcs w

inputEvent     :: Event -> World -> IO World
inputEvent (EventKey k Down _ pos) w
  | k == moveButton  = movePlayer 0 pos w
  | k == stopButton  = stopPlayer 0 w
inputEvent _                       w = return w

movePlayer :: Int -> Point -> World -> IO World
movePlayer name pos w =
  do walkingNPC (playerNpc (worldPlayer w)) pos
     announce (playerNpc (worldPlayer w))
     return w

stopPlayer :: Int -> World -> IO World
stopPlayer name w =
  do let pnpc = playerNpc (worldPlayer w)
     waitingNPC pnpc Nothing False
     announce pnpc
     return w

updateWorld    :: Float -> World -> IO World
updateWorld t w = do pnpc' <- updateNPC t False $ playerNpc $ worldPlayer w
                     npcs' <- mapM (updateNPC t True) $ worldNpcs w
                     return w { worldPlayer = (worldPlayer w) { playerNpc = pnpc' }
                              , worldNpcs = npcs' }

drawNPC :: NPC -> IO Picture
drawNPC npc =
  do state <- readIORef (npcState npc)
     return $ translate x y $ color (c state)
            $ pictures [ line [(0,0),end state], circle 10 ]

  where (x,y) = npcPos npc
        end (Walking w) = npcVelocity w * 10
        end _           = (0,0)

        c   Dead                       = red
        c   (Waiting w) | npcStunned w = yellow
        c   _                          = green




serverMain n = do
  sockets <- replicateM n accept
  w <- initWorld True
  announceWorld w sockets
  forM sockets $ \s ->
    forkIO $ handleSocket socket w
  runGame w
