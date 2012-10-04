{-# LANGUAGE RecordWildCards #-}
module Server (serverMain) where

import Control.Concurrent
import Control.Monad
import Graphics.Gloss.Data.Point
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

generateSetWorld :: ServerWorld -> ServerCommand
generateSetWorld w = SetWorld [(npcPos npc, npcFacing npc) | npc <- allNpcs w]

allNpcs :: ServerWorld -> [NPC]
allNpcs w = map playerNpc (serverPlayers w) ++ serverNpcs w

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

runServer :: [Handle] -> MVar ServerWorld -> IO a
runServer hs w = forever $
  do threadDelay (1000000 `div` eventsPerSecond)
     let period = recip $ fromIntegral eventsPerSecond
     modifyMVar_ w $ updateServerWorld hs period

announce :: ServerCommand -> [Handle] -> IO ()
announce msg hs = mapM_ (`hPutServerCommand` msg) hs

initServerWorld :: Int -> IO ServerWorld
initServerWorld playerCount =
  do serverPlayers <- mapM (initPlayer        ) [0 .. playerCount - 1]
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
