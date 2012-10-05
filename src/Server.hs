{-# LANGUAGE RecordWildCards #-}
module Server (serverMain) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List (intercalate, sortBy)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Prelude hiding (catch)
import Network
import System.IO

import Simulation
import NetworkMessages
import ListUtils

serverMain :: Int -> Int -> IO ()
serverMain numPlayers npcCount = do
  sock <- listenOn gamePort
  putStrLn $ "Server listening for ninjas on " ++ (showPort gamePort) ++ "."
  _ <- forkIO $ do
    (hs, names) <- getConnections sock numPlayers
    sClose sock

    (w,msgs) <- newGame [ (p,0) | p <- names ] npcCount
    var <- newMVar w

    -- these handles are only to be used for reading
    rawHs <- unsafeReadHandles hs
    forM_ rawHs $ \(i,h) ->
       forkIO $ clientSocketLoop i hs h var npcCount

    mapM_ (announce hs) msgs

    readyCountdown hs var

    runServer hs var

  _ <- getLine
  return ()

showPort :: PortID-> String
showPort (Service s) = "service " ++ s
showPort (PortNumber n) = "port " ++ show n
showPort (UnixSocket s) = "socket " ++ s

readyCountdown :: Handles -> MVar ServerWorld -> IO ()
readyCountdown hs var =
  do forM_ ["3","2","1", "Capture the Diamonds!"] $ \i ->
       do announce hs $ ServerMessage i
          threadDelay 1000000
     announce hs ServerReady
     modifyMVar_ var $ \w -> return w { serverMode = Playing }

newGame :: [(String,Int)] -> Int -> IO (ServerWorld, [ServerCommand])
newGame scores npcCount =
  do w   <- initServerWorld scores npcCount
     return (w, [ SetWorld [(npcPos npc, npcFacing npc) | npc <- allNpcs w] ])

allNpcs :: ServerWorld -> [NPC]
allNpcs w = map playerNpc (serverPlayers w) ++ serverNpcs w

isStuckPlayer :: Player -> Bool
isStuckPlayer p =
  case npcState (playerNpc p) of
    Dead          -> True
    Attacking {}  -> True
    _             -> False

clientSocketLoop :: Int -> Handles -> Handle -> MVar ServerWorld -> Int -> IO ()
clientSocketLoop i hs h var npcCount =
  forever processOne
  `catch` \ e ->
  putStrLn $ "Read loop: Socket error ("++show (e :: IOException)++"), dropping connection"
  where
  processOne = do
     msg  <- hGetClientCommand h
     join $ modifyMVar var $ \w ->
       let players = serverPlayers w
           (me,them) = extract i players
           mapPlayer f = w { serverPlayers = updateList i f (serverPlayers w) }
           returnAnd x m = return (x,m)

       in case msg of
            NewGame | serverMode w == Stopped ->
                        do (w',m) <- newGame (serverScores w) npcCount
                           returnAnd w' $ do forM_ m $ announce hs
                                             readyCountdown hs var

            _       | serverMode w /= Playing || isStuckPlayer me -> returnAnd w $ return ()

            ClientSmoke
              | hasSmokebombs me ->
                   let w' = mapPlayer consumeSmokebomb
                   in returnAnd w' $ announce hs $ ServerSmoke $ npcPos $ playerNpc me

            ClientCommand cmd -> case cmd of
              Move _ pos0
                -- Disregard where the player says he is moving from
                | pointInBox pos boardMin boardMax ->
                        let w' = mapPlayer $ mapPlayerNpc $ \npc -> walkingNPC npc pos
                        in returnAnd w' $ announce hs $ ServerCommand i $ Move (npcPos (playerNpc me)) pos
                where
                pos = constrainPoint (npcPos (playerNpc me)) pos0

              Stop     -> let w' = mapPlayer $ mapPlayerNpc $ \npc -> waitingNPC npc Nothing False
                          in returnAnd w' $ announce hs $ ServerCommand i cmd

              Attack   -> let (me', them', npcs', cmds, kills)
                                 = performAttack me them (serverNpcs w)
                              w' = w { serverPlayers = insertPlayer me' them'
                                     , serverNpcs    = npcs'
                                     }
                          in returnAnd w'
                            $ do forM_ cmds  $ announce hs
                                 forM_ kills $ \(killed,killer) ->
                                     announceOne hs killed
                                       $ ServerMessage $ "Killed by " ++ killer

              _        -> returnAnd w $ return ()
            _          -> returnAnd w $ return ()

serverScores :: ServerWorld -> [(String,Int)]
serverScores w = [ (playerUsername p, playerScore p) | p <- serverPlayers w ]

getConnections :: Socket -> Int -> IO (Handles,[String])
getConnections s n =
  do var <- newMVar []
     aux (Handles var) [] n
  where
  aux hs names 0 = return (hs, names)
  aux hs names i =
    do announce hs $ ServerWaiting i
       (h,host,port) <- accept s
       hSetBuffering h LineBuffering
       ClientJoin name <- hGetClientCommand h
       putStrLn $ "Got connection from " ++ name ++ "@" ++ host ++ ":" ++ show port
       let i' = i - 1
       addHandle i' h hs
       aux hs (name:names) i'

runServer :: Handles -> MVar ServerWorld -> IO a
runServer hs w = loop =<< getCurrentTime
  where
  loop lastTime =
    do thisTime <- getCurrentTime
       let elapsed :: Float
           elapsed = realToFrac $ diffUTCTime thisTime lastTime
       modifyMVar_ w $ updateServerWorld hs elapsed
       threadDelay $ truncate $ 1000000 / fromIntegral eventsPerSecond - elapsed
       loop thisTime

initServerWorld :: [(String,Int)] -> Int -> IO ServerWorld
initServerWorld scores npcCount =
  do let playerCount = length scores
     serverPlayers <- zipWithM initPlayer [0 ..] scores
     serverNpcs    <- mapM (initServerNPC True) [playerCount .. npcCount + playerCount - 1]
     let serverMode = Starting
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
              _             -> (filter isWinner pcs', "by capturing the diamonds!")

        pcs2 <-
         if null winners
           then return pcs'
           else do let ps = map (addVictory winners) pcs'
                   announce hs $ ServerMessage
                            $ commas (map playerUsername winners) ++ " wins " ++ reason
                   announce hs
                      $ ServerMessage
                      $ commas $ map prettyScore $ reverse $ sortBy (compare `on` playerScore) ps
                   return ps

        npcs' <- mapM (updateNPC hs t True) $ serverNpcs    w
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
  |  playerName p `elem` map playerName winners =
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
  do let (npc',mbTask) = updateNPC' t npc

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

newtype Handles = Handles (MVar [(Int,Handle)])

addHandle :: Int -> Handle -> Handles -> IO ()
addHandle i h (Handles var) = modifyMVar_ var $ \hs -> return ((i,h):hs)

unsafeReadHandles :: Handles -> IO [(Int,Handle)]
unsafeReadHandles (Handles var) = readMVar var

announceOne :: Handles -> Int -> ServerCommand -> IO ()
announceOne (Handles var) i msg =
  withMVar var             $ \hs ->
  for_ (lookup i hs)       $ \h ->
  handle ignoreIOException $
  hPutServerCommand h msg
  where
  -- Dead handles get cleaned up in 'announce'
  ignoreIOException :: IOException -> IO ()
  ignoreIOException _ = return ()

announce :: Handles -> ServerCommand -> IO ()
announce (Handles var) msg =
  modifyMVar_ var  $ \hs ->
  flip filterM hs  $ \(_name,h) ->
     do hPutServerCommand h msg
        return True
      `catch` \ e ->
     do putStrLn $ "announce: Socket error ("++show (e :: IOException)++"), dropping connection"
        return False
