{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Network
import System.Environment
import System.IO
import System.Random

import NetworkMessages

gamePort :: PortID
gamePort = PortNumber 16000

windowPadding :: Int
windowPadding = 20

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

moveButton, stopButton, attackButton :: Key
moveButton = MouseButton LeftButton
stopButton = MouseButton RightButton
attackButton = Char 'a'

npcCount :: Int
npcCount = 30

attackDistance :: Float
attackDistance = 100

attackAngle    :: Float
attackAngle    = pi/4

stunTime = 3


eventsPerSecond = 100

--------------------------------------------------------------------------------

main :: IO ()
main =
  do args <- getArgs
     case args of
       ["server",n]        -> serverMain (read n)
       ["client"]          -> clientMain "localhost"
       ["client",hostname] -> clientMain hostname
       _ -> do putStrLn "Server usage: Main server NUM_CLIENTS"
               putStrLn "Client usage: Main client [HOSTNAME]"

serverMain :: Int -> IO ()
serverMain n = do
  sock <- listenOn gamePort
  forkIO $ do
    hs <- getConnections sock n
    sClose sock
    w   <- initServerWorld n
    var <- newMVar w
    let setCommand = generateSetWorld w
    forM_ (zip [0..] hs) $ \(i,h) ->
      do hPutServerCommand h setCommand
         forkIO $ clientSocketLoop i hs h var
    runServer hs var
  _ <- getLine
  return ()

generateSetWorld w = SetWorld [(npcPos npc, npcFacing npc) | npc <- allNpcs w]

allNpcs w = map playerNpc (serverPlayers w) ++ serverNpcs w

extract 0 (x:xs) = (x,xs)
extract i (x:xs) =
  let (y,ys) = extract (i-1) xs
  in (y, x:ys)

clientSocketLoop :: Int -> [Handle] -> Handle -> MVar ServerWorld -> IO ()
clientSocketLoop i hs h var = forever $
  do cmd <- hGetCommand h
     putStrLn $ "Client command: " ++ show cmd
     msgs <- modifyMVar var $ \w ->
       do let players = serverPlayers w
              (me,them) = extract i players
              updatePlayerNpc f = w { serverPlayers = updateList i (mapPlayerNpc f) (serverPlayers w) }

          -- XXX: These should work only if the player is not dead!
          return $ case cmd of
            Move pos | pointInBox pos boardMin boardMax
                     -> ( updatePlayerNpc $ \npc -> walkingNPC npc pos
                        , [ServerCommand i cmd]
                        )
            Stop     -> ( updatePlayerNpc $ \npc -> waitingNPC npc Nothing False
                        , [ServerCommand i cmd]
                        )
            Attack   -> let (me', them', npcs', cmds) = performAttack me them (serverNpcs w)
                        in  (w { serverPlayers = insertPlayer me' them'
                               , serverNpcs    = npcs'
                               }
                            , cmds
                            )
            _        -> (w,[])
     forM_ msgs $ \msg -> announce msg hs

clientMain :: HostName -> IO ()
clientMain hostname =
  do h <- connectTo hostname gamePort
     hSetBuffering h LineBuffering

     poss <- getInitialWorld h
     r <- newMVar (initClientWorld poss)
     _ <- forkIO $ clientUpdates h r
     runGame h r

getConnections :: Socket -> Int -> IO [Handle]
getConnections s = aux []
  where
  aux hs 0 = return hs
  aux hs i =
    do announce (ServerWaiting i) hs
       (h,host,port) <- accept s
       hSetBuffering h LineBuffering
       putStrLn $ "Got connection from " ++ host ++ ":" ++ show port
       aux (h:hs) (i-1)

getInitialWorld :: Handle -> IO [(Point,Vector)]
getInitialWorld h =
  do msg <- hGetServerCommand h
     case msg of
       SetWorld poss -> return poss
       ServerWaiting n ->
         do putStrLn $ "Server waiting for " ++ show n ++ " more clients"
            getInitialWorld h
       _ -> fail "Unexpected initial message"

clientUpdates :: Handle -> MVar World -> IO ()
clientUpdates h var = forever $
  do ServerCommand name cmd <- hGetServerCommand h
     modifyMVar_ var $ \w ->
       return $ w { worldNpcs = updateList name (npcCommand cmd) $ worldNpcs w }
  where
  npcCommand cmd npc = case cmd of
    Move pos -> walkingNPC npc pos
    Stop     -> waitingNPC npc Nothing False
    Stun     -> stunnedNPC npc
    Die      -> deadNPC npc
    Attack   -> npc -- XXX need to set attack state

runGame :: Handle -> MVar World -> IO ()
runGame h var =
     playIO
       (InWindow "Ninjas" (round width + windowPadding, round height + windowPadding) (10,10))
       black
       eventsPerSecond
       () -- "state"
       (\() -> fmap drawWorld (readMVar var))
       (inputEvent h)
       (\t () -> modifyMVar_ var $ \w -> return $ updateClientWorld t w)
  where (width,height) = subPt boardMax boardMin

runServer :: [Handle] -> MVar ServerWorld -> IO a
runServer hs w = forever $
  do threadDelay (1000000 `div` eventsPerSecond)
     let period = recip $ fromIntegral eventsPerSecond
     modifyMVar_ w $ updateServerWorld hs period

addPt :: Point -> Point -> Point
addPt (x,y) (a,b) = (x+a,y+b)

subPt :: Point -> Point -> Point
subPt (x,y) (a,b) = (x-a,y-b)

data NPC      = NPC
  { npcName   :: Int
  , npcPos    :: Point
  , npcFacing :: Vector -- Unit vector
  , npcState  :: State
  }

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

data ServerWorld = ServerWorld
  { serverNpcs    :: [NPC]
  , serverPlayers :: [Player]
  }

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
  do degrees <- randomRIO (0,359) :: IO Int
     let rads = pi * fromIntegral degrees / 180 :: Float
     return (cos rads, sin rads)

vectorToAngle :: Vector -> Float
vectorToAngle (x,y)
  | x < 0     = pi + atan (y/x)
  | otherwise =      atan (y/x)
  

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



updateNPC :: [Handle] -> Float -> Bool -> NPC -> IO NPC
updateNPC hs t think npc =
  do let (npc',mbTask) = updateNPC' t npc

     case guard think >> mbTask of

       Just ChooseWait ->
         do time <- pickWaitTime True
            return $ waitingNPC npc' time False

       Just ChooseDestination ->
         do tgt <- randomBoardPoint
            announce (ServerCommand (npcName npc') (Move tgt)) hs
            return $ walkingNPC npc' tgt

       Nothing -> return npc'

announce msg hs =
  do mapM_ (`hPutServerCommand` msg) hs

initClientWorld :: [(Point, Vector)] -> World
initClientWorld poss =
  World { worldNpcs = zipWith initClientNPC [0..] poss }

initServerWorld :: Int -> IO ServerWorld
initServerWorld playerCount =
  do serverPlayers <- mapM (initPlayer        ) [0 .. playerCount - 1]
     serverNpcs    <- mapM (initServerNPC True) [playerCount .. npcCount + playerCount - 1]
     return ServerWorld { .. }

inputEvent     :: Handle -> Event -> () -> IO ()
inputEvent h (EventKey k Down _ pos) ()
  | k == moveButton   = hPutCommand h (Move pos)
  | k == stopButton   = hPutCommand h Stop
  | k == attackButton = hPutCommand h Attack
inputEvent _ _ () = return ()

updateClientWorld :: Float -> World -> World
updateClientWorld t w =
  w { worldNpcs = map (fst . updateNPC' t) $ worldNpcs w }

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

drawWorld      :: World -> Picture
drawWorld w     = pictures (borderPicture : map drawNPC (worldNpcs w))

borderPicture   = color red $ rectangleWire width height
  where (width,height) = subPt boardMax boardMin

drawNPC :: NPC -> Picture
drawNPC npc =
  let state = npcState npc
  in  translate x y $ color (c state)
            $ pictures [ line [(0,0), mulSV ninjaRadius $ npcFacing npc]
                       , circle ninjaRadius
                       , color white attackArc ]
  where
        (x,y) = npcPos npc

        c   Dead                       = red
        c   (Waiting w) | npcStunned w = yellow
        c   _                          = green

        attackArc = arcRad (rads - attackAngle) (rads + attackAngle) attackDistance
          where
          rads = vectorToAngle (npcFacing npc)

radToDeg rad = 180 / pi * rad

arcRad a b = arc (radToDeg a) (radToDeg b)

updateList 0 f (x:xs) = f x : xs
updateList i f (x:xs) = x : updateList (i-1) f xs
