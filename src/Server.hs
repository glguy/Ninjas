{-# LANGUAGE RecordWildCards #-}
module Server (ServerEnv(..), defaultServerEnv, serverMain) where

import Control.DeepSeq (rnf)
import Control.Concurrent (forkIO, threadDelay, ThreadId,
                           Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, handle, bracket_)
import Control.Monad
import Data.List (intercalate, sortBy, (\\))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Network
import Network.Socket (getSocketName)
import System.IO

import Simulation
import NetworkMessages

data ServerEnv = ServerEnv
  { npcCount          :: Int
  , initialSmokebombs :: Int
  , serverPort        :: Int
  , shutdownOnEmpty   :: Bool
  }

defaultServerEnv :: ServerEnv
defaultServerEnv = ServerEnv
  { npcCount          = 10
  , initialSmokebombs = 1
  , serverPort        = 16000
  , shutdownOnEmpty   = False
  }

serverMain :: ServerEnv -> IO ()
serverMain env = do
  events <- newChan
  _acceptThreadId <- startNetwork env events
  lastTick <- getCurrentTime
  _tickThreadId <- forkIO $ tickThread events
  w <- initServerWorld env []
  eventLoop env emptyHandles w events lastTick

startNetwork :: ServerEnv -> Chan ServerEvent -> IO ThreadId
startNetwork env events =
  do sock     <- listenOn (PortNumber (fromIntegral (serverPort env)))
     sockName <- getSocketName sock
     putStrLn $ "Server listening for ninjas on " ++ show sockName
     forkIO $ forM_ [0..] $ \i ->
       do (h,host,port) <- accept sock
          hSetBuffering h NoBuffering
          forkIO $
            do ClientJoin name <- hGetClientCommand h
               putStrLn $ concat ["Got connection from ",
                                  name, "@", host, ":", show port]
               bracket_ (writeChan events $ JoinEvent i name h)
                        (writeChan events $ DisconnectEvent i)
                        (clientSocketLoop i h events)

clientSocketLoop :: Int -> Handle -> Chan ServerEvent -> IO ()
clientSocketLoop i h events =
  handle ignoreIOException $
  forever $ do c <- hGetClientCommand h
               writeChan events $ ClientEvent i c

readyCountdown :: Handles -> ServerWorld -> IO ServerWorld
readyCountdown hs w =
  do forM_ ["3","2","1", "Capture the Diamonds!"] $ \txt ->
       do announce hs $ ServerMessage txt
          threadDelay 600000
     announce hs ServerReady
     return w { serverMode = Playing }

newGame :: ServerEnv -> ServerWorld -> IO (ServerWorld, [ServerCommand])
newGame env w =
  do let scores = serverScores w
               ++ [(i,u,0) | (i,u) <- serverLobby w]
     w' <- initServerWorld env scores
     return (w', [generateSetWorld w'])

generateSetWorld :: ServerWorld -> ServerCommand
generateSetWorld w =
  SetWorld [(npcName npc, npcPos npc, npcFacing npc) | npc <- allNpcs w]

allNpcs :: ServerWorld -> [NPC]
allNpcs w = map playerNpc (serverPlayers w) ++ serverNpcs w

isStuckPlayer :: Player -> Bool
isStuckPlayer p =
  case npcState (playerNpc p) of
    Dead          -> True
    Attacking {}  -> True
    _             -> False

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
       NewGame | serverMode w == Stopped
                 || not (isInLobby i w)
                    && length (serverPlayers w) == 1 ->
                        do (w',m) <- newGame env w
                           forM_ m $ announce hs
                           readyCountdown hs w'
               | otherwise -> return w

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
                  
                  let winningAttack = all isDeadPlayer them'
                  them'' <- if winningAttack
                            then endGame hs [me] them' "force"
                            else return them'

                  let mode
                        | winningAttack = Stopped
                        | otherwise     = Playing

                  return $ w { serverPlayers = me' : them''
                             , serverNpcs    = npcs'
                             , serverMode    = mode
                             }

           _        -> return w
       _          -> return w

isDeadPlayer :: Player -> Bool
isDeadPlayer p = Dead == npcState (playerNpc p)

serverScores :: ServerWorld -> [(Int,String,Int)]
serverScores w = [ (npcName (playerNpc p), playerUsername p, playerScore p)
                 | p <- serverPlayers w ]

tickThread :: Chan ServerEvent -> IO ()
tickThread events =
  forever $ do writeChan events TickEvent
               threadDelay $ 1000000 `div` eventsPerSecond

initServerWorld :: ServerEnv -> [(Int,String,Int)] -> IO ServerWorld
initServerWorld env scores =
  do let npcIds      = take (npcCount env)
                     $ [0..] \\ [i | (i,_,_) <- scores]
     let newPlayer   = initPlayer (initialSmokebombs env)
         serverMode  = Stopped
         serverLobby = []
     serverPlayers   <- mapM (\(i,u,s) -> newPlayer i u s) scores
     serverNpcs      <- mapM (initServerNPC True) npcIds
     return ServerWorld { .. }

updateServerWorld    :: Handles -> Float -> ServerWorld -> IO ServerWorld
updateServerWorld hs t w
  | serverMode w /= Playing = return w
  | otherwise =
     do pcs'  <- mapM (updatePlayer hs t) $ serverPlayers w
        npcs' <- mapM (updateNPC hs t True) $ serverNpcs w

        let winners = filter isWinner pcs'

        pcs2 <- if null winners
                then return pcs'
                else endGame hs winners pcs' "deception"

        let mode'
              | null winners       = Playing
              | otherwise          = Stopped

        return w { serverPlayers = pcs2
                 , serverNpcs    = npcs'
                 , serverMode    = mode'
                 }

endGame :: Handles -> [Player] -> [Player] -> String -> IO [Player]
endGame hs winners players reason =

  do -- Declare victory
     announce hs $ ServerMessage
       $ commas (map playerUsername winners) ++ " wins by " ++ reason ++ "!"

     -- Update scores
     let players' = map (addVictory winners) players

     -- Announce scores
     announce hs
       $ ServerMessage $ commas $ map prettyScore
       $ reverse $ sortBy (comparing playerScore) players'

     return players'

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
  let packet = mkServerPacket msg in
  for_ (lookupHandle i hs) $ \h ->
  handle ignoreIOException $
  hPutServerPacket h packet

-- Dead handles get cleaned up in 'announce'
ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

announce :: Handles -> ServerCommand -> IO ()
announce hs msg =
  let packet = mkServerPacket msg in
  forM_ (listHandles hs) $ \(_name,h) ->
     handle ignoreIOException $
     hPutServerPacket h packet

extractPlayer :: Int -> [Player] -> Maybe (Player, [Player])
extractPlayer _ [] = Nothing
extractPlayer i (p:ps)
  | npcName (playerNpc p) == i = return (p,ps)
  | otherwise = do (x,xs) <- extractPlayer i ps
                   return (x,p:xs)

data ServerEvent
  = TickEvent
  | JoinEvent
      Int
      String
      Handle
  | DisconnectEvent
      Int -- client id
  | ClientEvent
      Int -- client id
      ClientCommand -- received command

eventLoop ::
  ServerEnv ->
  Handles ->
  ServerWorld ->
  Chan ServerEvent ->
  UTCTime {- ^ time previous tick was processed -} ->
  IO ()
eventLoop env hs w events lastTick
  | rnf w `seq` lastTick `seq` False = undefined
  | shutdownOnEmpty env && nullHandles hs = return ()
  | otherwise      = logic =<< readChan events
  where
  logic (JoinEvent i user h) =
    do let hs' = addHandle i h hs
           w'  = w { serverLobby = (i,user):serverLobby w }
           env' = env { shutdownOnEmpty = True }
       announceOne hs' i $ generateSetWorld w
       forM_ (serverLobby w) $ \(_,u) ->
         announceOne hs' i $ ServerMessage $ u ++ " in lobby"
       forM_ (serverPlayers w) $ \p ->
         announceOne hs' i $ ServerMessage $ playerUsername p ++ " in game"
       announce hs' $ ServerMessage $ user ++ " joined lobby"
       eventLoop env' hs' w' events lastTick

  logic TickEvent =
    do now <- getCurrentTime
       let elapsed = realToFrac (diffUTCTime now lastTick)
       w' <- updateServerWorld hs elapsed w
       eventLoop env hs w' events now

  logic (ClientEvent name msg)
      -- The ids of lobby players might overlap with NPCs
    | isInLobby name w
        && msg /= NewGame = eventLoop env hs w events lastTick
    | otherwise =
        do w' <- updateWorldForCommand env name hs w msg
           eventLoop env hs w' events lastTick

  logic (DisconnectEvent name) =
    do let hs' = removeHandle name hs
       putStrLn $ "Client disconnect with id " ++ show name
       case extractPlayer name $ serverPlayers w of
         Just (p,ps) ->
           do announce hs' $ ServerCommand name Die
              announce hs' $ ServerMessage $ playerUsername p ++ " disconnected"
              
              when (null ps) $ announce hs $ ServerMessage "Game Over"

              let mode | null ps        = Stopped
                       | otherwise      = Playing

              let w' = w { serverPlayers = ps
                         , serverMode    = mode
                         }

              eventLoop env hs' w' events lastTick

         Nothing ->
           case lookup name $ serverLobby w of
             Just u ->
              do announce hs' $ ServerMessage $ u ++ " left lobby"
                 let w' = w { serverLobby = [(k,v) | (k,v) <- serverLobby w
                                                   , k /= name] }
                 eventLoop env hs' w' events lastTick
             Nothing -> -- This really shouldn't happen
              eventLoop env hs w events lastTick

isInLobby :: Int -> ServerWorld -> Bool
isInLobby i w = i `elem` map fst (serverLobby w)
