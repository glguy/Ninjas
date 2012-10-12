{-# LANGUAGE RecordWildCards #-}
module Server (ServerEnv(..), defaultServerEnv, serverMain) where

import Control.Monad (forM_, when, guard)
import Data.List (intercalate, sortBy, (\\))
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Network (PortID(..))

import Simulation
import Character
import NetworkMessages
import Parameters

import NetworkedGame.Server (NetworkServer(..), announce, announceOne, Handles)
import NetworkedGame.Handles (ConnectionId(..))
import qualified NetworkedGame.Server as NS

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

-- | Main entry point for server
serverMain :: ServerEnv -> IO ()
serverMain env =
  do w                  <- initServerWorld env []
     let ServerEnv { serverPort = port } = env
     let settings = NetworkServer
           { serverPort = PortNumber $ fromIntegral port
           , eventsPerSecond = Parameters.eventsPerSecond
           , onTick     = updateServerWorld
           , onConnect  = connect
           , onDisconnect = disconnect
           , onCommand = command env
           }
     NS.serverMain settings w

-----------------------------------------------------------------------
-- Game logic
-----------------------------------------------------------------------

readyCountdown :: Handles -> ServerWorld -> IO ServerWorld
readyCountdown hs w =
  do announce hs $ ServerMessage "3"
     return w { serverMode = Starting 0 }

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
                           announce hs $ generateSetWorld w'
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
                       announceOne hs (ConnectionId killed)
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

startingMode :: Handles -> Float -> Float -> ServerWorld -> IO ServerWorld
startingMode hs t duration w =
  do when (boundary (1*0.6)) $ announce hs $ ServerMessage "2"
     when (boundary (2*0.6)) $ announce hs $ ServerMessage "1"
     when (boundary (3*0.6)) $ announce hs $ ServerMessage "GO!"
     mode' <- if boundary (4*0.6)
                then do announce hs ServerReady
                        return Playing
                else return $ Starting duration'
     return $ w { serverMode = mode' }
  where
  duration'	= duration + t
  boundary x 	= duration < x && x <= duration'

updateServerWorld    :: Handles -> Float -> ServerWorld -> IO ServerWorld
updateServerWorld hs t w =
  case serverMode w of
    Stopped -> return w
    Starting duration -> startingMode hs t duration w
    Playing ->
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

extractPlayer :: Int -> [Player] -> Maybe (Player, [Player])
extractPlayer _ [] = Nothing
extractPlayer i (p:ps)
  | charName (playerCharacter p) == i = return (p,ps)
  | otherwise = do (x,xs) <- extractPlayer i ps
                   return (x,p:xs)

connect :: Handles -> ConnectionId -> ServerWorld -> IO ServerWorld
connect hs i w =
  do announceOne hs i $ generateSetWorld w
     return w

command :: ServerEnv -> Handles -> ConnectionId -> ClientCommand -> ServerWorld ->
  IO ServerWorld
command _ hs (ConnectionId i) (ClientJoin name) w = addClient hs i name w

command env hs (ConnectionId i) msg w
      -- The ids of lobby players might overlap with NPCs
    | isInLobby i w
        && msg /= NewGame = return w
    | otherwise = updateWorldForCommand env i hs w msg

disconnect :: Handles -> ConnectionId -> ServerWorld -> IO ServerWorld
disconnect hs (ConnectionId i) w =
  do putStrLn $ "Client disconnect with id " ++ show i
     case extractPlayer i $ serverPlayers w of
       Just (p,ps) ->
         do announce hs $ ServerCommand i Die
            announce hs $ ServerMessage $ playerUsername p ++ " disconnected"
            
            when (null ps) $ announce hs $ ServerMessage "Game Over"

            let mode | null ps        = Stopped
                     | otherwise      = Playing

            return w { serverPlayers = ps
                     , serverMode    = mode
                     }

       Nothing ->
         case lookup i $ serverLobby w of
           Just u ->
            do announce hs $ ServerMessage $ u ++ " left lobby"
               return w { serverLobby = [(k,v) | (k,v) <- serverLobby w
                                                 , k /= i] }
           Nothing -> return w

addClient :: Handles -> Int -> String -> ServerWorld -> IO ServerWorld
addClient hs i name w =
  do forM_ (serverLobby w) $ \(_,u) ->
         announceOne hs (ConnectionId i) $ ServerMessage $ u ++ " in lobby"
     forM_ (serverPlayers w) $ \p ->
         announceOne hs (ConnectionId i) $ ServerMessage $ playerUsername p ++ " in game"
     announce hs $ ServerMessage $ name ++ " joined lobby"
     return $ w { serverLobby = (i,name) : serverLobby w }
 -- XXX : Check that the client isn't already registered

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
