module Client (ClientEnv(..), defaultClientEnv, clientMain) where

import Control.Concurrent
import Control.Monad
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.IO
import Network
import Server (ServerEnv(..), defaultServerEnv)
import NetworkMessages

import ListUtils
import Simulation

moveButton, stopButton, attackButton, smokeButton, newGameButton :: Key
moveButton = MouseButton LeftButton
stopButton = MouseButton RightButton
attackButton = Char 'a'
smokeButton  = Char 's'
newGameButton = Char 'n'

windowPadding :: Int
windowPadding = 60

dingPeriod :: Float
dingPeriod = 1

smokePeriod :: Float
smokePeriod = 5

smokeRadius :: Float
smokeRadius = 125

dingScale :: Float
dingScale = 0.25

dingPosition :: Point
dingPosition = (fst boardMin + 5, snd boardMax - 20)

data ClientEnv = ClientEnv
  { hostname   :: HostName
  , clientPort :: Int
  , username   :: String
  }

defaultClientEnv :: ClientEnv
defaultClientEnv = ClientEnv
  { hostname   = "localhost"
  , clientPort = (serverPort defaultServerEnv)
  , username   = "Anon"
  }

clientMain :: ClientEnv -> IO ()
clientMain (ClientEnv host port name) =
  do h <- connectTo host (PortNumber (fromIntegral port))
     hSetBuffering h LineBuffering

     hPutClientCommand h (ClientJoin name)

     poss <- getInitialWorld h
     r <- newMVar (initClientWorld poss)
     _ <- forkIO $ clientUpdates h r
     runGame h r

serverWaitingMessage :: Int -> String
serverWaitingMessage n =
  "Server waiting for " ++ show n ++ " more ninja" ++ if (n > 1) then "s." else "."

getInitialWorld :: Handle -> IO [(Point,Vector)]
getInitialWorld h =
  do msg <- hGetServerCommand h
     case msg of
       SetWorld poss -> return poss
       ServerWaiting n ->
         do putStrLn $ serverWaitingMessage n
            getInitialWorld h
       _ -> fail "Unexpected initial message"

initClientWorld :: [(Point, Vector)] -> World
initClientWorld poss =
  World { worldNpcs = zipWith initClientNPC [0..] poss
        , dingTimers = []
        , worldMessages = []
        , smokeTimers = []
        }

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
drawWorld w     = pictures
                $ borderPicture
                : dingPicture (length (dingTimers w))
                : messagePictures (worldMessages w)
                : map drawPillar pillars
               ++ map (drawNPC (smokeTimers w)) (worldNpcs w)
               ++ map drawSmoke (smokeTimers w)

drawSmoke :: (Float, Point) -> Picture
drawSmoke (t, pt)
  = translateV pt
  $ rotate (radToDeg t)
  $ scale scalar scalar
  $ pictures
      [ color (greyN 0.1 ) $ circleSolid 1
      , color (greyN 0.25)
         $ pictures [ circle      1
                    , line [ (0,1) , (0,-1)
                           ]
                    , line [ (1,0) , (-1,0)
                           ]
                    ]
      ]
  where
  scalar = smokeRadiusScalar t

smokeRadiusScalar :: Float -> Float
smokeRadiusScalar t = smokeRadius * sin (t / (smokePeriod / pi))

messagePictures :: [String] -> Picture
messagePictures msgs = pictures (zipWith messagePicture [0..] msgs)

messagePicture :: Int -> String -> Picture
messagePicture i msg
  = translate (fst boardMin + 5) (snd boardMin + 5 + textHeight * fromIntegral i)
  $ scale 0.25 0.25
  $ color (greyN gray)
  $ text msg
  where
  textHeight = 40
  gray = (4 - fromIntegral (min 3 i)) / 4
  

dingPicture :: Int -> Picture
dingPicture n =
  pictures [ translateV dingPosition
           $ translate (5 * fromIntegral i) (- 5 * fromIntegral i)
           $ scale dingScale dingScale
           $ color white
           $ text "DING"
           | i <- [0..n-1]]

drawPillar :: Point -> Picture
drawPillar pt
  = translateV pt
  $ color cyan
  $ rectangleWire pillarSize pillarSize

borderPicture  :: Picture
borderPicture   = color red $ rectangleWire (2 * ninjaRadius + width) (2 * ninjaRadius + height)
  where (width,height) = subPt boardMax boardMin

drawNPC :: [(Float, Point)] -> NPC -> Picture
drawNPC smokes npc
  | covered = blank
  | otherwise
    = translateV (npcPos npc)
    $ rotate (negate $ radToDeg rads)
    $ color c
    $ pictures [ attackArc
               , circle ninjaRadius
               , scale ninjaRadius ninjaRadius wedge
               ]
  where state = npcState npc

        smokeCovering (t,pt) = magV (subPt pt (npcPos npc)) + ninjaRadius <= smokeRadiusScalar t
        covered = any smokeCovering smokes

        c = case state of
            Dead                       -> red
            (Attacking _)              -> purple
            (Waiting w) | npcStunned w -> yellow
            _                          -> green

        purple = makeColor8 0xa0 0x20 0xf0 0xff

        rads = argV $ npcFacing npc

        wedge =
          line [ rotateV attackAngle (1, 0)
               , (0,0)
               , rotateV (negate attackAngle) (1, 0)
               ]

        attackArc
          | state == Dead = blank
          | otherwise
              = color (greyN 0.2)
              $ pictures
                  [ scale attackDistance attackDistance wedge
                  , arcRad (negate attackAngle) attackAngle attackDistance
                  ]

-- | Translate a picture using a 'Vector'
translateV :: Vector -> Picture -> Picture
translateV (x,y) = translate x y

-- | Same as 'arc' but with angles measured in radians.
arcRad :: Float -> Float -> Float -> Picture
arcRad a b = arc (radToDeg a) (radToDeg b)

inputEvent     :: Handle -> Event -> () -> IO ()
inputEvent h (EventKey k Down _ pos) ()
  | k == moveButton   = hPutClientCommand h (ClientCommand (Move (0,0) pos))
  | k == stopButton   = hPutClientCommand h (ClientCommand Stop      )
  | k == attackButton = hPutClientCommand h (ClientCommand Attack    )
  | k == smokeButton  = hPutClientCommand h (ClientSmoke             )
  | k == newGameButton = hPutClientCommand h NewGame
inputEvent _ _ () = return ()

updateClientWorld :: Float -> World -> World
updateClientWorld d w =
  w { worldNpcs = map (fst . updateNPC' d) $ worldNpcs w
    , dingTimers  = [ t - d      |  t     <- dingTimers  w, t > d]
    , smokeTimers = [(t - d, pt) | (t,pt) <- smokeTimers w, t > d]
    }

clientUpdates :: Handle -> MVar World -> IO ()
clientUpdates h var = forever $
  do c <- hGetServerCommand h
     case c of
       ServerReady -> modifyMVar_ var $ \w -> return $ w { worldMessages = [] }
       ServerCommand name cmd ->
         modifyMVar_ var $ \w ->
           return $ w { worldNpcs = updateList name (npcCommand cmd) $ worldNpcs w }
       ServerMessage txt -> modifyMVar_ var $ \w -> return $ w { worldMessages = txt : worldMessages w }
       ServerDing        -> modifyMVar_ var $ \w -> return $ w { dingTimers = dingPeriod : dingTimers w }
       ServerSmoke pt    -> modifyMVar_ var $ \w -> return $ w { smokeTimers = (smokePeriod, pt) : smokeTimers w }
       SetWorld poss ->
        modifyMVar_ var $ \_ -> return $ initClientWorld poss
       _ -> return ()
  where
  npcCommand cmd npc = case cmd of
    Move from to -> walkingNPC npc { npcPos = from } to 
    Stop     -> waitingNPC npc Nothing False
    Stun     -> stunnedNPC npc
    Die      -> deadNPC npc
    Attack   -> attackNPC npc

