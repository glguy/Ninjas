module Client (clientMain) where

import Control.Concurrent
import Control.Monad
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Environment
import System.IO
import Network
import NetworkMessages

import ListUtils
import Simulation

moveButton, stopButton, attackButton :: Key
moveButton = MouseButton LeftButton
stopButton = MouseButton RightButton
attackButton = Char 'a'

windowPadding :: Int
windowPadding = 60


clientMain :: HostName -> IO ()
clientMain hostname =
  do h <- connectTo hostname gamePort
     hSetBuffering h LineBuffering

     name <- getEnv "USER"
     hPutClientCommand h (ClientJoin name)

     poss <- getInitialWorld h
     r <- newMVar (initClientWorld poss)
     _ <- forkIO $ clientUpdates h r
     runGame h r

getInitialWorld :: Handle -> IO [(Point,Vector)]
getInitialWorld h =
  do msg <- hGetServerCommand h
     case msg of
       SetWorld poss -> return poss
       ServerWaiting n ->
         do putStrLn $ "Server waiting for " ++ show n ++ " more clients"
            getInitialWorld h
       _ -> fail "Unexpected initial message"

initClientWorld :: [(Point, Vector)] -> World
initClientWorld poss =
  World { worldNpcs = zipWith initClientNPC [0..] poss }

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

drawWorld      :: World -> Picture
drawWorld w     = pictures (borderPicture : map drawNPC (worldNpcs w))

borderPicture  :: Picture
borderPicture   = color red $ rectangleWire (2 * ninjaRadius + width) (2 * ninjaRadius + height)
  where (width,height) = subPt boardMax boardMin

drawNPC :: NPC -> Picture
drawNPC npc =
  let state = npcState npc
  in  translate x y
    $ rotate (negate $ radToDeg rads)
    $ color (c state)
    $ pictures [ attackArc
               , circle ninjaRadius
               , scale ninjaRadius ninjaRadius wedge
               ]
  where
        (x,y) = npcPos npc

        c   Dead                       = red
        c   (Attacking _)              = purple
        c   (Waiting w) | npcStunned w = yellow
        c   _                          = green

        purple = makeColor8 0xa0 0x20 0xf0 0xff

        rads = argV $ npcFacing npc

        wedge =
          line [ rotateV attackAngle (1, 0)
               , (0,0)
               , rotateV (negate attackAngle) (1, 0)
               ]

        attackArc
          | npcState npc == Dead = blank
          | otherwise
              = color (greyN 0.5)
              $ pictures
                  [ scale attackDistance attackDistance wedge
                  , arcRad (negate attackAngle) attackAngle attackDistance
                  ]

-- | Same as 'arc' but with angles measured in radians.
arcRad :: Float -> Float -> Float -> Picture
arcRad a b = arc (radToDeg a) (radToDeg b)

inputEvent     :: Handle -> Event -> () -> IO ()
inputEvent h (EventKey k Down _ pos) ()
  | k == moveButton   = hPutClientCommand h (ClientCommand (Move pos))
  | k == stopButton   = hPutClientCommand h (ClientCommand Stop      )
  | k == attackButton = hPutClientCommand h (ClientCommand Attack    )
inputEvent _ _ () = return ()

updateClientWorld :: Float -> World -> World
updateClientWorld t w =
  w { worldNpcs = map (fst . updateNPC' t) $ worldNpcs w }

clientUpdates :: Handle -> MVar World -> IO ()
clientUpdates h var = forever $
  do c <- hGetServerCommand h
     case c of
       ServerCommand name cmd ->
         modifyMVar_ var $ \w ->
           return $ w { worldNpcs = updateList name (npcCommand cmd) $ worldNpcs w }
       ServerMessage txt -> putStrLn txt
       _ -> return ()
  where
  npcCommand cmd npc = case cmd of
    Move pos -> walkingNPC npc pos
    Stop     -> waitingNPC npc Nothing False
    Stun     -> stunnedNPC npc
    Die      -> deadNPC npc
    Attack   -> attackNPC npc
