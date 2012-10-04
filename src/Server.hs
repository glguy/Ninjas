{-# LANGUAGE RecordWildCards #-}
module Server (serverMain) where

import Control.Concurrent
import Control.Monad
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Network
import System.IO

import Simulation
import NetworkMessages
import ListUtils

npcCount :: Int
npcCount = 1

serverMain :: Int -> IO ()
serverMain n = do
  sock <- listenOn gamePort
  _ <- forkIO $ do
    conns <- getConnections sock n
    let (hs,names) = unzip conns
    sClose sock
    w   <- initServerWorld n names
    var <- newMVar w
    let setCommand = generateSetWorld w
    forM_ (zip [0..] hs) $ \(i,h) ->
      do hPutServerCommand h setCommand
         forkIO $ clientSocketLoop i hs h var
    runServer hs var
  _ <- getLine
  return ()

generateSetWorld :: ServerWorld -> ServerCommand
generateSetWorld w = SetWorld [(npcPos npc, npcFacing npc) | npc <- allNpcs w]

allNpcs :: ServerWorld -> [NPC]
allNpcs w = map playerNpc (serverPlayers w) ++ serverNpcs w

isStuckPlayer :: Player -> Bool
isStuckPlayer p =
  case npcState (playerNpc p) of
    Dead          -> True
    Attacking {}  -> True
    _             -> False

clientSocketLoop :: Int -> [Handle] -> Handle -> MVar ServerWorld -> IO ()
clientSocketLoop i hs h var = forever $
  do ClientCommand cmd <- hGetClientCommand h
     putStrLn $ "Client command: " ++ show cmd
     msgs <- modifyMVar var $ \w -> return $
       let players = serverPlayers w
           (me,them) = extract i players
           updatePlayerNpc f = w { serverPlayers = updateList i (mapPlayerNpc f) (serverPlayers w) }

       in case cmd of
            _        | isStuckPlayer me -> (w,[])
            Move pos0 ->
              let pos = constrainPoint (npcPos (playerNpc me)) pos0
              in if pointInBox pos boardMin boardMax
                 then   ( updatePlayerNpc $ \npc -> walkingNPC npc pos
                        , [ServerCommand i (Move pos)]
                        )
                 else   (w, [])
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

getConnections :: Socket -> Int -> IO [(Handle,String)]
getConnections s = aux []
  where
  aux hs 0 = return hs
  aux hs i =
    do announce (ServerWaiting i) (map fst hs)
       (h,host,port) <- accept s
       hSetBuffering h LineBuffering
       ClientJoin name <- hGetClientCommand h
       putStrLn $ "Got connection from " ++ name ++ "@" ++ host ++ ":" ++ show port
       aux ((h,name):hs) (i-1)

runServer :: [Handle] -> MVar ServerWorld -> IO a
runServer hs w = forever $
  do threadDelay (1000000 `div` eventsPerSecond)
     let period = recip $ fromIntegral eventsPerSecond
     modifyMVar_ w $ updateServerWorld hs period

announce :: ServerCommand -> [Handle] -> IO ()
announce msg hs = mapM_ (`hPutServerCommand` msg) hs

initServerWorld :: Int -> [String] -> IO ServerWorld
initServerWorld playerCount names =
  do serverPlayers <- zipWithM initPlayer [0 ..] names
     serverNpcs    <- mapM (initServerNPC True) [playerCount .. npcCount + playerCount - 1]
     return ServerWorld { .. }

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

constrainPoint :: Point -> Point -> Point
constrainPoint from
  = aux intersectSegVertLine (fst boardMin)
  . aux intersectSegVertLine (fst boardMax)
  . aux intersectSegHorzLine (snd boardMin)
  . aux intersectSegHorzLine (snd boardMax)
  where
  aux f x p = fromMaybe p (f from p x)
