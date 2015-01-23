{-# LANGUAGE StandaloneDeriving, RecordWildCards, DeriveDataTypeable #-}
module Bot
    ( -- * For internal use by Ninjas
      BotEnv(..)
    , Bot(..)
    , defaultBotEnv
    , botMain
    -- * Helper functionallity for custom bots
    , worldOf
    -- ** Types
    , BotHandle, BotEvent(..), InfoWorld(..)
    , ClientCharacter(..), Character(..)
    , State(..), WalkInfo(..), WaitInfo(..)
    -- ** Bot commands
    , move, stop, attack, start, smoke
    ) where

import Control.Concurrent
import Control.Concurrent.STM.TBChan as C
import Control.Concurrent.STM (atomically)
import Control.Monad
import qualified Control.Exception as X
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.IO
import Network
import Server (ServerEnv(..), defaultServerEnv)
import NetworkMessages
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Time
import System.Exit (exitSuccess)
import Data.Data
import Data.Dynamic

import Character
import Parameters
import VectorUtils
import qualified Anim

import Language.Haskell.Interpreter

windowPadding :: Int
windowPadding = 60

dingPeriod :: Float
dingPeriod = 1

textScale :: Float
textScale = 0.25

dingPosition :: Point
dingPosition = (fst boardMin + 5, snd boardMax - 20)

data BotHandle = BotHandle Handle
        deriving (Typeable)

data BotEnv = BotEnv
  { hostname   :: HostName
  , clientPort :: Int
  , botname    :: String
  , bot        :: Bot
  , runDisplay :: Bool
  }

defaultBotEnv :: BotEnv
defaultBotEnv = BotEnv
  { hostname   = "localhost"
  , clientPort = serverPort defaultServerEnv
  , botname    = "[Bot]Anon"
  , bot        = defaultBot
  , runDisplay = False
  }

data Bot = BotFile FilePath
         | BotNoop

defaultBot :: Bot
defaultBot = BotNoop

botMain :: BotEnv -> IO ()
botMain (BotEnv host port botname bot runDisplay) =
  do anim <- Anim.loadWorld
     h <- connectTo host (PortNumber (fromIntegral port))
     hSetBuffering h NoBuffering

     cmdChan <- C.newTBChanIO 100
     let bh = BotHandle h

     hPutClientCommand h (ClientJoin botname)

     poss <- getInitialWorld h
     r <- newMVar (initBotWorld anim poss)
     _ <- forkIO  $ botUpdates h cmdChan  r
     _ <- forkIO' $ runBot bot bh cmdChan r
     if runDisplay
         then showGame h r
         else don'tShowGame h r

serverWaitingMessage :: Int -> String
serverWaitingMessage n
  = "Server waiting for "
 ++ show n ++ " more ninja"
 ++ if n > 1 then "s." else "."

getInitialWorld :: Handle -> IO [(Int,Point,Vector)]
getInitialWorld h =
  do msg <- hGetServerCommand h
     case msg of
       SetWorld poss -> return poss
       ServerWaiting n ->
         do putStrLn $ serverWaitingMessage n
            getInitialWorld h
       _ -> fail "Unexpected initial message"

initBotWorld :: Anim.World -> [(Int, Point, Vector)] -> World
initBotWorld anim poss = World vw iw
  where

  iw :: InfoWorld
  iw = InfoWorld (IntMap.map clientCharacter (worldCharacters vw)) []

  vw :: VisibleWorld
  vw = VisibleWorld
        { worldCharacters = IntMap.fromList
                            [(i,initClientCharacter (Anim.npc anim) p v)
                                | (i,p,v) <- poss ]
        , dingTimers    = []
        , worldMessages = []
        , smokeTimers   = []
        , appearance    = anim
        }

forkIO' :: IO () -> IO ThreadId
forkIO' f = do
    me <- myThreadId
    forkFinally f (\e -> print e >> X.throwTo me X.ThreadKilled)

don'tShowGame :: Handle -> MVar World -> IO ()
don'tShowGame _h var = do
    let quitter = do
            c <- getChar
            when (c == 'q' || c == 'Q') exitSuccess
            quitter
    _ <- forkIO' quitter
    t <- getCurrentTime
    go t
  where
  go old = do
    threadDelay (1000000 `div` eventsPerSecond)
    now <- getCurrentTime
    let diff = realToFrac (diffUTCTime now old)
    modifyMVar_ var (return . updateBotWorld diff)
    go now

showGame :: Handle -> MVar World -> IO ()
showGame _h var =
     playIO
       (InWindow "Ninjas"
         (round width + windowPadding, round height + windowPadding)
         (10,10))
       black
       eventsPerSecond
       () -- "state"
       (\() -> fmap (drawWorld . visibleWorld) (readMVar var))
       inputEvent
       (\t () -> modifyMVar_ var $ \w -> return $ updateBotWorld t w)
  where (width,height) = subPt boardMax boardMin

drawWorld      :: VisibleWorld -> Picture
drawWorld w     = pictures
                $ borderPicture
                : dingPicture (length (dingTimers w))
                : map (drawPillar w) pillars
               ++ map drawCharacter (IntMap.elems $ worldCharacters w)
               ++ map drawSmoke (smokeTimers w)
               ++ messagePictures (worldMessages w)

inputEvent :: Event -> () -> IO ()
inputEvent (EventKey (Char 'Q') Down _ _) _ = exitSuccess
inputEvent _ _ = return ()

drawSmoke :: (Point, Anim.Animation) -> Picture
drawSmoke (pt,a) = translateV pt (Anim.curFrame a)

messagePictures :: [String] -> [Picture]
messagePictures msgs = zipWith messagePicture [0..] msgs

messagePicture :: Int -> String -> Picture
messagePicture i msg
  = translate (fst boardMin + 5)
              (snd boardMin + 5 + textHeight * fromIntegral i)
  $ scale textScale textScale
  $ color (greyN gray)
  $ text msg
  where
  textHeight = 40
  gray = (4 - fromIntegral (min 3 i)) / 4

dingPicture :: Int -> Picture
dingPicture n =
  pictures [ translateV dingPosition
           $ translate (5 * fromIntegral i) (- 5 * fromIntegral i)
           $ scale textScale textScale
           $ color white
           $ text "DING"
           | i <- [0..n-1]]

drawPillar :: VisibleWorld -> Point -> Picture
drawPillar w pt
  = translateV pt
  $ Anim.curFrame $ Anim.tower $ appearance w

borderPicture  :: Picture
borderPicture   = color red $ rectangleWire (2 * ninjaRadius + width)
                                            (2 * ninjaRadius + height)
  where (width,height) = subPt boardMax boardMin

drawCharacter :: ClientCharacter -> Picture
drawCharacter c
    = translateV (charPos char)
    $ rotate (negate $ radToDeg rads)
    $ Anim.curFrame (clientAnim c)
  where char  = clientCharacter c
        rads  = argV (charFacing char)

-- | Translate a picture using a 'Vector'
translateV :: Vector -> Picture -> Picture
translateV (x,y) = translate x y

data BotEvent = Cmd (ServerCommand,InfoWorld) | Tick InfoWorld
        deriving (Data,Typeable)

worldOf :: BotEvent -> InfoWorld
worldOf (Cmd (_,x)) = x
worldOf (Tick x)    = x

runBot :: Bot -> BotHandle -> TBChan ServerCommand -> MVar World -> IO ()
runBot BotNoop     _ _  _   = return ()
runBot (BotFile f) h cc var = do
  union <- newTBChanIO 100
  let timer = do
        threadDelay 50000
        w <- readMVar var
        atomically (writeTBChan union (Tick (infoWorld w)))
      cmd = do
          c <- atomically (readTBChan cc)
          w <- readMVar var
          atomically (writeTBChan union (Cmd (c,infoWorld w)))
  _ <- forkIO (forever cmd)
  _ <- forkIO (forever timer)
  e <- runInterpreter $
         do loadModules [f]
            setImports ["Prelude","Ninja","Bot","Data.Dynamic"]
            let ty :: BotHandle -> BotEvent -> Dynamic -> IO Dynamic
                ty = undefined
                tyBotWorld :: Dynamic
                tyBotWorld = undefined
            func <- interpret "ninja" ty
            botW <- interpret "ninjaStance" tyBotWorld
            let botLoop botWorld = do
                 tick  <- atomically (readTBChan union)
                 botW' <- func h tick botWorld
                 botLoop botW'
            liftIO $ botLoop botW
  putStrLn "Bot interpreter exited"
  case e of
    Left err -> error (show err)
    Right _  -> putStrLn "Bot exited normally"

updateBotWorld :: Float -> World -> World
updateBotWorld d (World w inf) = World vw iw
  where
  iw = InfoWorld (IntMap.map clientCharacter (worldCharacters vw))
                 [(p, t - d) | (p,t) <- smokeLocations inf, t > d]
  vw = w
    { worldCharacters = fmap (stepClientCharacter npcLooks d) (worldCharacters w)
    , dingTimers  = [ t - d      |  t     <- dingTimers  w, t > d]
    , smokeTimers = [(pt, Anim.update d a) |
                            (pt,a) <- smokeTimers w, not (Anim.finished d a) ]
    , appearance  = Anim.updateWorld d (appearance w)
    }
  npcLooks = Anim.npc (appearance w)

stepClientCharacter :: Anim.NPC -> Float -> ClientCharacter -> ClientCharacter
stepClientCharacter looks elapsed clientChar =
  case stepCharacter elapsed (clientCharacter clientChar) of
    (char,changed,_)
      | changed   -> newClientCharacter looks char
      | otherwise -> clientChar { clientCharacter = char
                                , clientAnim = Anim.update elapsed (clientAnim clientChar) }

newClientCharacter :: Anim.NPC -> Character -> ClientCharacter
newClientCharacter looks clientCharacter = ClientCharacter { .. }
  where
  clientAnim = case charState clientCharacter of
                 Walking {}               -> Anim.walk looks
                 Waiting w | waitStunned w -> Anim.stun   looks
                           | otherwise    -> Anim.stay   looks
                 Attacking {}             -> Anim.attack looks
                 Dead                     -> Anim.die    looks

botUpdates :: Handle -> TBChan ServerCommand -> MVar World -> IO ()
botUpdates h cc var = forever $
  do c <- hGetServerCommand h
     modifyMVar_ var $ \w -> return $! processCmd w c
     atomically $ writeTBChan cc c

  where

  processCmd wld@(World w inf) c =
    let g x = World x inf in
    case c of
      ServerReady       -> g w { worldMessages = [] }
      ServerMessage txt -> g w { worldMessages = txt : worldMessages w }
      ServerDing        -> g w { dingTimers = dingPeriod : dingTimers w }
      ServerSmoke pt    -> let smokeA = Anim.smoke (appearance w)
                           in g w { smokeTimers = (pt,smokeA) : smokeTimers w }
      SetWorld poss     -> initBotWorld (appearance $ visibleWorld wld) poss
      ServerCommand i m -> let f = npcCommand w m
                           in g w { worldCharacters = updateNpcList i f $ worldCharacters w }
      _                 -> wld

  npcCommand w cmd cnpc =
    let npc = clientCharacter cnpc
    in newClientCharacter (Anim.npc $ appearance w) $
       case cmd of
         Move from to   -> walkingCharacter to npc { charPos = from }
         Stop           -> waitingCharacter Nothing False npc
         Stun           -> stunnedCharacter npc
         Die            -> deadCharacter npc
         Attack         -> attackingCharacter npc

updateNpcList ::
  Int ->
  (ClientCharacter -> ClientCharacter) ->
  IntMap ClientCharacter -> IntMap ClientCharacter
updateNpcList i f = IntMap.update (Just . f) i

data VisibleWorld = VisibleWorld
  { worldCharacters  :: IntMap ClientCharacter
  , dingTimers       :: [Float]
  , smokeTimers      :: [(Point, Anim.Animation)]
  , worldMessages    :: [String]
  , appearance       :: Anim.World
  } deriving (Data,Typeable)

data InfoWorld = InfoWorld
  { infoCharacters    :: IntMap Character
  , smokeLocations    :: [(Point,Float)]
  } deriving (Data,Typeable)

data World = World
  { visibleWorld     :: VisibleWorld
  , infoWorld        :: InfoWorld
  } deriving (Data, Typeable)

-- | Construct a new character given a name, a position,
-- and a facing unit vector. This function is used
-- by clients who are told the parameters by the
-- server.
initClientCharacter :: Anim.NPC -> Point -> Vector -> ClientCharacter
initClientCharacter anim charPos charFacing =
  let charState = Waiting Wait { waitWaiting = Nothing, waitStunned = False }
      clientAnim = Anim.stay anim
      clientCharacter = Character { .. }
  in  ClientCharacter { .. }

data ClientCharacter = ClientCharacter
  { clientCharacter  :: Character
  , clientAnim       :: Anim.Animation
  } deriving (Data, Typeable)

-- Helper Functionallity for Bots

-- Bot Commands
bSendCommand :: BotHandle -> ClientCommand -> IO ()
bSendCommand (BotHandle h) = hPutClientCommand h

move     :: BotHandle -> Point -> IO ()
move h   = bSendCommand h . ClientCommand . Move (0,0)

stop     :: BotHandle -> IO ()
stop h   = bSendCommand h (ClientCommand Stop)

attack   :: BotHandle -> IO ()
attack h = bSendCommand h (ClientCommand Attack)

smoke    :: BotHandle -> IO ()
smoke h  = bSendCommand h ClientSmoke

start    :: BotHandle -> IO ()
start h  = bSendCommand h NewGame
