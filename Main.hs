{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Control.Monad
import Control.Concurrent
import System.IO
import Data.IORef
import Network

gamePort = PortNumber 16000

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

npcCount = 25


eventsPerSecond = 100

--------------------------------------------------------------------------------

main :: IO ()
main =
  do serverMain 1
     putStrLn "Starting client"
     clientMain "localhost"

serverMain :: Int -> IO ()
serverMain n = do
  listenH <- listenOn gamePort
  forkIO $ do
    conns <- replicateM n (accept listenH)
    let hs = [h | (h,_,_) <- conns]
    w  <- initServerWorld n
    let setCommand = SetWorld (map npcPos (map playerNpc (serverPlayers w) ++ serverNpcs w))
    mapM_ (`hSetBuffering` NoBuffering) hs
    mapM_ (\h -> hPrint h setCommand) hs
    -- XXX: listen for client events
    runServer hs w
  return ()


clientMain hostname =
  do h <- connectTo hostname gamePort
     hSetBuffering h NoBuffering
     SetWorld poss <- readIO =<< hGetLine h
     r <- newIORef =<< initClientWorld poss
     _ <- forkIO $ clientUpdates h r
     runGame r

clientUpdates :: Handle -> IORef World -> IO ()
clientUpdates h r = forever $
  do ServerCommand name cmd <- readIO =<< hGetLine h
     w <- readIORef r
     let npc = worldNpcs w !! name
     case cmd of
       Move pos -> walkingNPC npc pos
       Stop     -> waitingNPC npc Nothing False
       Stun     -> waitingNPC npc Nothing True

runGame :: IORef World -> IO ()
runGame r =
     readIORef r >>= \w ->
     playIO
       (InWindow "test" (round width, round height) (10,10)) black eventsPerSecond
       w
       drawWorld
       inputEvent
       (updateClientWorld r)
  where (width,height) = subPt boardMax boardMin

runServer hs w =
  do threadDelay (1000000 `div` eventsPerSecond)
     w' <- updateServerWorld hs (recip $ fromIntegral eventsPerSecond) w
     runServer hs w'

data ServerWorld = ServerWorld
  { serverNpcs    :: [NPC]
  , serverPlayers :: [Player]
  }

announceWorld :: World -> IO ()
announceWorld w = print () -- XXX

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
  { worldNpcs        :: [NPC]
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

initClientNPC :: Int -> Point -> IO NPC
initClientNPC npcName npcPos =
  do npcState <- newIORef (Waiting Wait { npcWaiting = Nothing, npcStunned = False })
     return NPC { .. }

initServerNPC :: Bool -> Int -> IO NPC
initServerNPC think npcName =
  do npcPos   <- randomBoardPoint
     npcWaiting <- pickWaitTime think
     let npcStunned = False
     npcState <- newIORef (Waiting Wait { .. })
     return NPC { .. }

initPlayer :: Int -> IO Player
initPlayer name =
  do playerNpc <- initServerNPC False name
     return Player { .. }

pickWaitTime :: Bool -> IO (Maybe Float)
pickWaitTime False = return Nothing
pickWaitTime True  = fmap Just $ randomRIO (0, restTime)


data ThinkTask = ChooseWait | ChooseDestination

{- This performs one time tick update of an NPC.  It is important
that we perofrm this operation atomically to avoid a race condition
with external threads that try to modify the state (e.g., user input
or commands from the server).  In addition to returning an updated NPC,
we also return a kind of "continutaion" telling us what needs to be
done next for computer controlled characters. -}
updateNPC' :: Float -> NPC -> IO (NPC, Maybe ThinkTask)
updateNPC' elapsed npc = atomicModifyIORef (npcState npc) $ \state ->
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

  where done next = ( Waiting Wait { npcWaiting = Nothing, npcStunned = False }
                    , (npc, Just next)
                    )
        working s n = (s,(n,Nothing))



updateNPC :: [Handle] -> Float -> Bool -> NPC -> IO NPC
updateNPC hs t think npc =
  do (npc',mbTask) <- updateNPC' t npc

{- Note that if we need to think, then we are a computer contorolled
NPC and so ther should be no external threads---such as user input,
or instructions from the server---that can modify the state.
This is why updating the state in the code bellow does not lead
to a race condition. -}

     when think $
       case mbTask of

         Just ChooseWait ->
           do time <- pickWaitTime True
              waitingNPC npc' time False

         Just ChooseDestination ->
            do tgt <- randomBoardPoint
               walkingNPC npc' tgt
               mapM_ (announce (ServerCommand (npcName npc') (Move tgt))) hs

         Nothing -> return ()

     return npc'

announce msg h =
  do print msg
     hPrint h msg

initClientWorld :: [Point] -> IO World
initClientWorld poss =
  do npcs <- zipWithM initClientNPC [0..] poss
     return World { worldNpcs = npcs }

initServerWorld :: Int -> IO ServerWorld
initServerWorld playerCount =
  do serverPlayers <- mapM initPlayer [0 .. playerCount - 1]
     serverNpcs    <- mapM (initServerNPC True) [playerCount .. npcCount + playerCount - 1]
     return ServerWorld { .. }

drawWorld      :: World -> IO Picture
drawWorld w     = fmap pictures $ mapM drawNPC $ worldNpcs w

inputEvent     :: Event -> World -> IO World
inputEvent (EventKey k Down _ pos) w
  | k == moveButton  = sendClientCommand (Move pos)
  | k == stopButton  = sendClientCommand Stop
inputEvent _                       w = return w

sendClientCommand cmd = error "sendClientCommand"

updateClientWorld :: IORef World -> Float -> World -> IO World
updateClientWorld r t w =
  do npcs' <- mapM (updateNPC [] t False) $ worldNpcs w
     let w1 = w { worldNpcs = npcs' }
     writeIORef r w1
     return w1

updateServerWorld    :: [Handle] -> Float -> ServerWorld -> IO ServerWorld
updateServerWorld hs t w =
  do pcs'  <- mapM (updatePlayer hs t)   $ serverPlayers w
     npcs' <- mapM (updateNPC hs t True) $ serverNpcs    w
     return w { serverPlayers = pcs'
              , serverNpcs    = npcs'
              }

updatePlayer :: [Handle] -> Float -> Player -> IO Player
updatePlayer hs t p =
  do npc' <- updateNPC hs t False $ playerNpc p
     return p { playerNpc = npc' }

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


data Command
  = Move Point
  | Stop
  | Attack
  | Stun
  | Die
  deriving (Show, Read, Eq)
  
data ServerCommand
  = ServerCommand Int Command
  | SetWorld [Point]
  deriving (Show, Read)
