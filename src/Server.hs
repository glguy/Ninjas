{-# LANGUAGE RecordWildCards #-}
module Server (ServerEnv(..), defaultServerEnv, serverMain) where

import Control.DeepSeq (rnf)
import Control.Concurrent (forkIO, threadDelay, ThreadId,
                           Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, handle, bracket_)
import Control.Monad (forM_, when, guard, forever)
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
import Character
import NetworkMessages
import Parameters

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

-- | Main entry point for server
serverMain :: ServerEnv -> IO ()
serverMain env =
  do events             <- newChan
     _acceptThreadId    <- startNetwork env events
     lastTick           <- getCurrentTime
     _tickThreadId      <- forkIO $ tickThread events
     w                  <- initServerWorld env []
     eventLoop env emptyHandles w events lastTick

-----------------------------------------------------------------------
-- Client/server IO
-----------------------------------------------------------------------

-- | Create a thread which will accept new connections.
-- Connections and disconnections will be announced to the event channel.
startNetwork :: ServerEnv -> Chan ServerEvent -> IO ThreadId
startNetwork env events =
  do sock       <- listenOn (PortNumber (fromIntegral (serverPort env)))
     sockName   <- getSocketName sock
     putStrLn $ "Server listening for ninjas on " ++ show sockName
     forkIO $ mapM_ (acceptClient events sock) [0..]

-- | Accept a connection and create a thread to manage incoming data
-- from that connection.
acceptClient :: Chan ServerEvent -> Socket -> Int -> IO ThreadId
acceptClient events sock i =
  do (h,host,port) <- accept sock
     hSetBuffering h NoBuffering
     forkIO $
       do ClientJoin name <- hGetClientCommand h
          putStrLn $ concat ["Got connection from ",
                                  name, "@", host, ":", show port]
          bracket_ (writeChan events $ JoinEvent i name h)
                   (writeChan events $ DisconnectEvent i)
                   (clientSocketLoop i h events)

-- | Read incoming packets, decode them, and pass them along to
-- the event channel.
clientSocketLoop :: Int -> Handle -> Chan ServerEvent -> IO ()
clientSocketLoop i h events =
  handle ignoreExceptions $
  forever $ do c <- hGetClientCommand h
               writeChan events $ ClientEvent i c

-- | Send a command to a collection of clients
announce :: Handles -> ServerCommand -> IO ()
announce hs msg =
  let packet = mkServerPacket msg in
  forM_ (listHandles hs) $ \(_name,h) ->
     handle ignoreExceptions $
     hPutServerPacket h packet

-- | Send a command to a single client identified by id.
announceOne ::
  Handles ->
  Int {- ^ client id -} ->
  ServerCommand ->
  IO ()
announceOne hs i msg =
  let packet = mkServerPacket msg in
  for_ (lookupHandle i hs) $ \h ->
  handle ignoreExceptions $
  hPutServerPacket h packet

ignoreExceptions :: SomeException -> IO ()
ignoreExceptions _ = return ()

-----------------------------------------------------------------------
-- Game logic
-----------------------------------------------------------------------

readyCountdown :: Handles -> ServerWorld -> IO ServerWorld
readyCountdown hs w =
  do forM_ ["3","2","1", "Capture the Diamonds!"] $ \txt ->
       do announce hs $ ServerMessage txt
          threadDelay 600000
     announce hs ServerReady
     return w { serverMode = Playing }

-- | Construct a new game world preserving the scores from
-- the previous world and adding the lobby players in.
newGame :: ServerEnv -> ServerWorld -> IO ServerWorld
newGame env w =
  do let scores = serverScores w ++ [(i,u,0) | (i,u) <- serverLobby w]
     initServerWorld env scores

generateSetWorld :: ServerWorld -> ServerCommand
generateSetWorld w =
  SetWorld [(charName char, charPos char, charFacing char) | char <- allCharacters w]

allCharacters :: ServerWorld -> [Character]
allCharacters w = map playerCharacter (serverPlayers w) ++ serverNpcs w

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
         mapMyNpc = mapPlayer . mapPlayerCharacter

     case msg of
       NewGame | serverMode w == Stopped
                 || not (isInLobby i w)
                    && length (serverPlayers w) == 1 ->
                        do w' <- newGame env w
                           announce hs $ generateSetWorld w
                           readyCountdown hs w'
               | otherwise -> return w

       _       | serverMode w /= Playing || isStuckPlayer me -> return w

       ClientSmoke
         | hasSmokebombs me ->
           do announce hs $ ServerSmoke $ charPos $ playerCharacter me
              return $ mapPlayer consumeSmokebomb

       ClientCommand cmd ->
         case cmd of
           Move _ pos0
                -- Disregard where the player says he is moving from
             | pointInBox pos boardMin boardMax ->
               do announce hs $ ServerCommand i
                    $ Move (charPos (playerCharacter me)) pos
                  return $ mapMyNpc $ walkingCharacter pos
             where
             pos = constrainPoint (charPos (playerCharacter me)) pos0

           Stop     ->
               do announce hs $ ServerCommand i cmd
                  return $ mapMyNpc $ waitingCharacter Nothing False

           Attack   ->
               do let (me', them', npcs', cmds, kills)
                        = performAttack me them (serverNpcs w)
                  forM_ cmds  $ announce hs
                  forM_ kills $ \killed ->
                    do let killer = playerUsername me
                       announceOne hs killed
                         $ ServerMessage $ "Killed by " ++ killer
                  
                  let winningAttack = all isDeadPlayer them'
                  everyone <- if winningAttack
                            then endGame hs [me'] (me' : them') "force"
                            else return $ me' : them'

                  let mode
                        | winningAttack = Stopped
                        | otherwise     = Playing

                  return $ w { serverPlayers = everyone
                             , serverNpcs    = npcs'
                             , serverMode    = mode
                             }

           _        -> return w
       _          -> return w

serverScores :: ServerWorld -> [(Int,String,Int)]
serverScores w = [ (charName (playerCharacter p), playerUsername p, playerScore p)
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
     serverNpcs      <- mapM (initServerCharacter True) npcIds
     return ServerWorld { .. }

updateServerWorld    :: Handles -> Float -> ServerWorld -> IO ServerWorld
updateServerWorld hs t w
  | serverMode w /= Playing = return w
  | otherwise =
     do pcs'  <- mapM (playerLogic    hs t     ) $ serverPlayers w
        npcs' <- mapM (characterLogic hs t True) $ serverNpcs w

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
playerName = charName . playerCharacter

playerLogic :: Handles -> Float -> Player -> IO Player
playerLogic hs t p =
  do char <- characterLogic hs t False $ playerCharacter p
     let p' = p { playerCharacter = char }
     case whichPillar (charPos char) of
       Just i | i `notElem` playerVisited p -> 
         do announce hs ServerDing
            return p' { playerVisited = i : playerVisited p' }
       _ -> return p'

characterLogic :: Handles -> Float -> Bool -> Character -> IO Character
characterLogic hs t think char =
  do let (char',_,mbTask) = stepCharacter t char

     case guard think >> mbTask of

       Just ChooseWait ->
         do time <- pickWaitTime True
            return $ waitingCharacter time False char'

       Just ChooseDestination ->
         do tgt <- randomBoardPoint
            announce hs $ ServerCommand (charName char') (Move (charPos char) tgt)
            return $ walkingCharacter tgt char'

       Nothing -> return char'

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

extractPlayer :: Int -> [Player] -> Maybe (Player, [Player])
extractPlayer _ [] = Nothing
extractPlayer i (p:ps)
  | charName (playerCharacter p) == i = return (p,ps)
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

-----------------------------------------------------------------------
-- Player predicates
-----------------------------------------------------------------------

isInLobby :: Int -> ServerWorld -> Bool
isInLobby i w = i `elem` map fst (serverLobby w)

isStuckPlayer :: Player -> Bool
isStuckPlayer p =
  case charState (playerCharacter p) of
    Dead          -> True
    Attacking {}  -> True
    _             -> False

isDeadPlayer :: Player -> Bool
isDeadPlayer p = Dead == charState (playerCharacter p)

isWinner :: Player -> Bool
isWinner p = length (playerVisited p) == length pillars
