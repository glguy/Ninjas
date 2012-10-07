{-# LANGUAGE RecordWildCards #-}
module Server (ServerEnv(..), defaultServerEnv, serverMain) where

import Control.DeepSeq (rnf)
import Control.Concurrent (forkIO, threadDelay,
                           Chan, newChan, readChan, writeChan)
import Control.Exception
import Control.Monad
import Data.List (intercalate, sortBy)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Prelude hiding (catch)
import Network
import Network.Socket (getSocketName)
import System.IO

import Simulation
import NetworkMessages

data ServerEnv = ServerEnv
  { npcCount          :: Int
  , initialSmokebombs :: Int
  , serverPort        :: Int
  }

defaultServerEnv :: ServerEnv
defaultServerEnv = ServerEnv
  { npcCount          = 10
  , initialSmokebombs = 1
  , serverPort        = 16000
  }

serverMain :: ServerEnv -> Int -> IO ()
serverMain env n = do

  (hs, names) <- startNetwork env n

  (w,msgs) <- newGame env [ (p,0) | p <- names ]
  events <- newChan

  -- these handles are only to be used for reading
  forM_ (listHandles hs) $ \(i,h) -> forkIO $ clientSocketLoop i h events
  mapM_ (announce hs) msgs

  w' <- readyCountdown hs w
  lastTick <- getCurrentTime
  _tickThreadId <- forkIO $ tickThread events
  eventLoop env hs w' events lastTick

startNetwork :: ServerEnv -> Int -> IO (Handles, [String])
startNetwork env n =
  do sock     <- listenOn (PortNumber (fromIntegral (serverPort env)))
     sockName <- getSocketName sock
     putStrLn $ "Server listening for ninjas on " ++ show sockName
     (hs, names) <- getConnections sock n
     sClose sock
     return (hs, names)

readyCountdown :: Handles -> ServerWorld -> IO ServerWorld
readyCountdown hs w =
  do forM_ ["3","2","1", "Capture the Diamonds!"] $ \txt ->
       do announce hs $ ServerMessage txt
          threadDelay 600000
     announce hs ServerReady
     return w { serverMode = Playing }

newGame :: ServerEnv -> [(String,Int)] -> IO (ServerWorld, [ServerCommand])
newGame env scores =
  do w <- initServerWorld env scores
     return (w, [ SetWorld [(npcPos npc, npcFacing npc) | npc <- allNpcs w] ])

allNpcs :: ServerWorld -> [NPC]
allNpcs w = map playerNpc (serverPlayers w) ++ serverNpcs w

isStuckPlayer :: Player -> Bool
isStuckPlayer p =
  case npcState (playerNpc p) of
    Dead          -> True
    Attacking {}  -> True
    _             -> False

clientSocketLoop :: Int -> Handle -> Chan ServerEvent -> IO ()
clientSocketLoop i h events =
  forever (do c <- hGetClientCommand h
              writeChan events (ClientEvent i c))
  `catch` \(SomeException _) ->
  writeChan events (ClientDisconnect i)
     

updateWorldForCommand ::
  ServerEnv ->
  Int {- ^ ID of sender -} ->
  Handles ->
  ServerWorld ->
  ClientCommand ->
  IO ServerWorld
updateWorldForCommand env i hs w msg =
  do let (me,them) = fromMaybe (error "clientSocketLoop")
                   $ extractPlayer i $ serverPlayers w
         mapPlayer f = w { serverPlayers = f me : them }
         mapMyNpc = mapPlayer . mapPlayerNpc

     case msg of
       NewGame | serverMode w == Stopped ->
                        do (w',m) <- newGame env $ serverScores w
                           forM_ m $ announce hs
                           readyCountdown hs w'

       _       | serverMode w /= Playing || isStuckPlayer me -> return w

       ClientSmoke
         | hasSmokebombs me ->
           do announce hs $ ServerSmoke $ npcPos $ playerNpc me
              return $ mapPlayer consumeSmokebomb

       ClientCommand cmd ->
         case cmd of
           Move _ pos0
                -- Disregard where the player says he is moving from
             | pointInBox pos boardMin boardMax ->
               do announce hs $ ServerCommand i
                    $ Move (npcPos (playerNpc me)) pos
                  return $ mapMyNpc $ \npc -> walkingNPC npc pos
             where
             pos = constrainPoint (npcPos (playerNpc me)) pos0

           Stop     ->
               do announce hs $ ServerCommand i cmd
                  return $ mapMyNpc $ \npc -> waitingNPC npc Nothing False

           Attack   ->
               do let (me', them', npcs', cmds, kills)
                        = performAttack me them (serverNpcs w)
                  forM_ cmds  $ announce hs
                  forM_ kills $ \killed ->
                    do let killer = playerUsername me
                       announceOne hs killed
                         $ ServerMessage $ "Killed by " ++ killer
                  return $ w { serverPlayers = me' : them'
                             , serverNpcs    = npcs'
                             }

           _        -> return w
       _          -> return w

serverScores :: ServerWorld -> [(String,Int)]
serverScores w = [ (playerUsername p, playerScore p) | p <- orderedPlayers ]
  where
  -- Players need to be ordered so they will work with the next
  -- when mapped with initPlayer in the next game
  orderedPlayers = sortBy (comparing (npcName . playerNpc)) (serverPlayers w)

getConnections :: Socket -> Int -> IO (Handles,[String])
getConnections s n =
  do aux emptyHandles [] n
  where
  aux hs names 0 = return (hs, names)
  aux hs names i =
    do announce hs $ ServerWaiting i
       (h,host,port) <- accept s
       hSetBuffering h LineBuffering
       ClientJoin name <- hGetClientCommand h
       putStrLn $ concat ["Got connection from ",
                          name, "@", host, ":", show port]
       let i' = i - 1
       aux (addHandle i' h hs) (name:names) i'

tickThread :: Chan ServerEvent -> IO ()
tickThread events =
  forever $ do writeChan events ServerTick
               threadDelay $ 1000000 `div` eventsPerSecond

initServerWorld :: ServerEnv -> [(String,Int)] -> IO ServerWorld
initServerWorld env scores =
  do let playerCount = length scores
     let newPlayer   = initPlayer (initialSmokebombs env)
     serverPlayers   <- zipWithM newPlayer [0 ..] scores
     serverNpcs      <- mapM (initServerNPC True)
                             [playerCount .. npcCount env + playerCount - 1]
     let serverMode  = Starting
     return ServerWorld { .. }

updateServerWorld    :: Handles -> Float -> ServerWorld -> IO ServerWorld
updateServerWorld hs t w
  | serverMode w /= Playing = return w
  | otherwise =
     do pcs'  <- mapM (updatePlayer hs t) $ serverPlayers w

        let survivors = filter (not . (Dead ==) . npcState . playerNpc) pcs'
            (winners, reason) = case (survivors, pcs') of
              -- This match on pcs' ensures single-player
              -- games don't immediately terminate
              ([_],(_:_:_)) -> (survivors, "by murder!")
              _             -> (filter isWinner pcs',
                                 "by capturing the diamonds!")

        pcs2 <-
         if null winners
           then return pcs'
           else do let ps = map (addVictory winners) pcs'
                   announce hs $ ServerMessage
                     $ commas (map playerUsername winners)
                       ++ " wins " ++ reason
                   announce hs
                     $ ServerMessage $ commas $ map prettyScore
                     $ reverse $ sortBy (comparing playerScore) ps
                   return ps

        npcs' <- mapM (updateNPC hs t True) $ serverNpcs w
        return w { serverPlayers = pcs2
                 , serverNpcs    = npcs'
                 , serverMode    = if null winners then Playing else Stopped
                 }

prettyScore :: Player -> String
prettyScore p = playerUsername p ++ ": " ++ show (playerScore p)

commas :: [String] -> String
commas = intercalate ", "

addVictory :: [Player] -> Player -> Player
addVictory winners p
  | playerName p `elem` map playerName winners =
                                p { playerScore = 1 + playerScore p }
  | otherwise = p


playerName :: Player -> Int
playerName = npcName . playerNpc

updatePlayer :: Handles -> Float -> Player -> IO Player
updatePlayer hs t p =
  do npc' <- updateNPC hs t False $ playerNpc p
     let p' = p { playerNpc = npc' }
     case whichPillar (npcPos npc') of
       Just i | i `notElem` playerVisited p -> 
         do announce hs ServerDing
            return p' { playerVisited = i : playerVisited p' }
       _ -> return p'

isWinner :: Player -> Bool
isWinner p = length (playerVisited p) == length pillars

updateNPC :: Handles -> Float -> Bool -> NPC -> IO NPC
updateNPC hs t think npc =
  do let (npc',_,mbTask) = updateNPC' t npc

     case guard think >> mbTask of

       Just ChooseWait ->
         do time <- pickWaitTime True
            return $ waitingNPC npc' time False

       Just ChooseDestination ->
         do tgt <- randomBoardPoint
            announce hs $ ServerCommand (npcName npc') (Move (npcPos npc) tgt)
            return $ walkingNPC npc' tgt

       Nothing -> return npc'

constrainPoint :: Point -> Point -> Point
constrainPoint from
  = aux intersectSegVertLine (fst boardMin)
  . aux intersectSegVertLine (fst boardMax)
  . aux intersectSegHorzLine (snd boardMin)
  . aux intersectSegHorzLine (snd boardMax)
  where
  aux f x p = fromMaybe p (f from p x)

newtype Handles = Handles { listHandles :: [(Int,Handle)] }

emptyHandles :: Handles
emptyHandles = Handles []

addHandle :: Int -> Handle -> Handles -> Handles
addHandle i h (Handles hs) = Handles ((i,h):hs)

removeHandle :: Int -> Handles -> Handles
removeHandle i (Handles hs) = Handles (aux hs)
  where
  aux [] = []
  aux (x:xs)
    | i == fst x = xs
    | otherwise  = x : aux xs

lookupHandle :: Int -> Handles -> Maybe Handle
lookupHandle i (Handles xs) = lookup i xs

nullHandles :: Handles -> Bool
nullHandles (Handles xs) = null xs

announceOne :: Handles -> Int -> ServerCommand -> IO ()
announceOne hs i msg =
  for_ (lookupHandle i hs) $ \h ->
  handle ignoreIOException $
  hPutServerCommand h msg

-- Dead handles get cleaned up in 'announce'
ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

announce :: Handles -> ServerCommand -> IO ()
announce hs msg =
  forM_ (listHandles hs) $ \(_name,h) ->
     handle ignoreIOException $
     hPutServerCommand h msg

extractPlayer :: Int -> [Player] -> Maybe (Player, [Player])
extractPlayer _ [] = Nothing
extractPlayer i (p:ps)
  | npcName (playerNpc p) == i = return (p,ps)
  | otherwise = do (x,xs) <- extractPlayer i ps
                   return (x,p:xs)

data ServerEvent
  = ServerTick
  | ClientDisconnect
      Int -- client id
  | ClientEvent
      Int -- client id
      ClientCommand -- received command

eventLoop ::
  ServerEnv ->
  Handles ->
  ServerWorld ->
  Chan ServerEvent ->
  UTCTime {- ^ time previous ServerTick was processed -} ->
  IO ()
eventLoop env hs w events lastTick
  | rnf w `seq` lastTick `seq` False = undefined
  | nullHandles hs = return ()
  | otherwise      = logic =<< readChan events
  where
  logic ServerTick =
    do now <- getCurrentTime
       let elapsed = realToFrac (diffUTCTime now lastTick)
       w' <- updateServerWorld hs elapsed w
       eventLoop env hs w' events now

  logic (ClientEvent name msg) =
    do w' <- updateWorldForCommand env name hs w msg
       eventLoop env hs w' events lastTick

  logic (ClientDisconnect name) =
    do let hs' = removeHandle name hs
       putStrLn $ "Client disconnect with id " ++ show name
       eventLoop env hs' w events lastTick
