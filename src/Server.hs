{-# LANGUAGE RecordWildCards #-}
module Server (ServerEnv(..), defaultServerEnv, serverMain) where

import Control.Applicative (Applicative)
import Control.Monad (when, guard)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import Data.List (intercalate, sortBy, (\\))
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Traversable (sequenceA)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Line
import Network (PortID(..))
import qualified Data.IntMap as IntMap

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

data ServerWorld = ServerWorld
  { serverNpcs    :: IntMap Character
  , serverPlayers :: IntMap Player
  , serverMode    :: ServerMode
  , serverLobby   :: [(Int,String)]
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
  SetWorld [(i, charPos char, charFacing char) | (i,char) <- allCharacters w]

allCharacters :: ServerWorld -> [(Int,Character)]
allCharacters w = IntMap.toList (fmap playerCharacter (serverPlayers w))
               ++ IntMap.toList (serverNpcs w)

updateWorldForCommand ::
  ServerEnv ->
  Int {- ^ ID of sender -} ->
  Handles ->
  ServerWorld ->
  ClientCommand ->
  IO ServerWorld
updateWorldForCommand env i hs w msg =
  do let Just me = IntMap.lookup i $ serverPlayers w
         myPos = charPos $ playerCharacter me
         mapPlayer f = w { serverPlayers =
                           IntMap.update (Just . f) i (serverPlayers w) }
         mapMyNpc = mapPlayer . mapPlayerCharacter

     case msg of
       NewGame | serverMode w == Stopped
                 || not (isInLobby i w)
                    && IntMap.size (serverPlayers w) == 1 ->
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
                    $ Move myPos pos
                  return $ mapMyNpc $ walkingCharacter pos
             where
             pos = constrainPoint myPos pos0

           Stop     ->
               do announce hs $ ServerCommand i cmd
                  return $ mapMyNpc $ waitingCharacter Nothing False

           Attack   ->
               do performAttack hs i w
           _        -> return w
       _          -> return w

serverScores :: ServerWorld -> [(Int,String,Int)]
serverScores w = [ (i, playerUsername p, playerScore p)
                 | (i,p) <- IntMap.toList $ serverPlayers w ]

initServerWorld :: ServerEnv -> [(Int,String,Int)] -> IO ServerWorld
initServerWorld env scores =
  do let npcIds      = take (npcCount env)
                     $ [0..] \\ [i | (i,_,_) <- scores]
     let newPlayer   = initPlayer (initialSmokebombs env)
         serverMode  = Stopped
         serverLobby = []
     serverPlayers   <- fmap IntMap.fromList
                      $ mapM (\(i,u,s) -> fmap ((,) i) (newPlayer u s)) scores
     serverNpcs      <- fmap IntMap.fromList
                      $ mapM (\i -> fmap ((,) i) (initServerCharacter True)) npcIds
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
     do pcs'  <- mapWithKeyA (playerLogic hs t) $ serverPlayers w
        npcs' <- mapWithKeyA (characterLogic hs t True) $ serverNpcs w

        let winners = IntMap.filter isWinner pcs'

        pcs2 <- if IntMap.null winners
                then return pcs'
                else endGame hs winners pcs' "deception"

        let mode'
              | IntMap.null winners = Playing
              | otherwise          = Stopped

        return w { serverPlayers = pcs2
                 , serverNpcs    = npcs'
                 , serverMode    = mode'
                 }

endGame :: Handles -> IntMap Player -> IntMap Player -> String ->
  IO (IntMap Player)
endGame hs winners players reason =

  do -- Declare victory
     announce hs $ ServerMessage
       $ commas (map playerUsername (IntMap.elems winners))
          ++ " wins by " ++ reason ++ "!"

     -- Update scores
     let players' = IntMap.mapWithKey (addVictory (IntMap.keys winners)) players

     -- Announce scores
     announce hs
       $ ServerMessage $ commas $ map prettyScore
       $ reverse $ sortBy (comparing playerScore) $ IntMap.elems players'

     return players'

prettyScore :: Player -> String
prettyScore p = playerUsername p ++ ": " ++ show (playerScore p)

commas :: [String] -> String
commas = intercalate ", "

addVictory :: [Int] -> Int -> Player -> Player
addVictory winners name p
  | name `elem` winners = p { playerScore = 1 + playerScore p }
  | otherwise = p

playerLogic :: Handles -> Float -> Int -> Player -> IO Player
playerLogic hs t name p =
  do char <- characterLogic hs t False name $ playerCharacter p
     let p' = p { playerCharacter = char }
     case whichPillar (charPos char) of
       Just i | i `notElem` playerVisited p ->
         do announce hs ServerDing
            return p' { playerVisited = i : playerVisited p' }
       _ -> return p'

characterLogic :: Handles -> Float -> Bool -> Int -> Character -> IO Character
characterLogic hs t think charName char =
  do let (char',_,mbTask) = stepCharacter t char

     case guard think >> mbTask of

       Just ChooseWait ->
         do time <- pickWaitTime True
            return $ waitingCharacter time False char'

       Just ChooseDestination ->
         do tgt <- randomBoardPoint
            announce hs $ ServerCommand charName
                        $ Move (charPos char) tgt
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

connect :: Handles -> ConnectionId -> ServerWorld -> IO ServerWorld
connect hs i w =
  do announceOne hs i $ generateSetWorld w
     return w

command ::
  ServerEnv ->
  Handles ->
  ConnectionId {- ^ player who sent command -} ->
  ClientCommand ->
  ServerWorld ->
  IO ServerWorld
command _   hs (ConnectionId i) (ClientJoin name) w = addClient hs i name w
command env hs (ConnectionId i) msg w
      -- The ids of lobby players might overlap with NPCs
    | isInLobby i w
        && msg /= NewGame = return w
    | otherwise = updateWorldForCommand env i hs w msg

disconnect :: Handles -> ConnectionId -> ServerWorld -> IO ServerWorld
disconnect hs (ConnectionId i) w =
  do putStrLn $ "Client disconnect with id " ++ show i
     case IntMap.lookup i $ serverPlayers w of
       Just p ->
         do let ps = IntMap.delete i $ serverPlayers w
            announce hs $ ServerCommand i Die
            announce hs $ ServerMessage $ playerUsername p ++ " disconnected"

            when (IntMap.null ps) $ announce hs $ ServerMessage "Game Over"

            let mode | IntMap.null ps = Stopped
                     | otherwise      = serverMode w

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
  do for_ (serverLobby w) $ \(_,u) ->
         announceOne hs (ConnectionId i) $ ServerMessage $ u ++ " in lobby"
     for_ (serverPlayers w) $ \p ->
         announceOne hs (ConnectionId i) $ ServerMessage $ playerUsername p ++ " in game"
     announce hs $ ServerMessage $ name ++ " joined lobby"
     return $ w { serverLobby = (i,name) : serverLobby w }
 -- XXX : Check that the client isn't already registered

performAttack :: Handles -> Int -> ServerWorld -> IO ServerWorld
performAttack hs attackId w =
  do let Just attacker = IntMap.lookup attackId $ serverPlayers w
         them = IntMap.delete attackId $ serverPlayers w
     let attackChar = playerCharacter attacker

     announce hs $ ServerCommand attackId Attack

     let me' = mapPlayerCharacter attackingCharacter attacker
     them' <- mapWithKeyA (attackPlayer hs attacker) them
     npcs' <- mapWithKeyA (attackCharacter hs attackChar) (serverNpcs w)

     let winningAttack = all isDeadPlayer $ IntMap.elems them'
     everyone <- if winningAttack
                   then endGame hs (IntMap.singleton attackId me')
                                   (IntMap.insert attackId me' them') "force"
                   else return $ IntMap.insert attackId me' them'

     let mode
           | winningAttack = Stopped
           | otherwise     = Playing

     return $ w { serverPlayers = everyone
                , serverNpcs    = npcs'
                , serverMode    = mode
                }

attackCharacter :: Handles -> Character -> Int -> Character -> IO Character
attackCharacter hs attacker targetId target
  | canHitPoint attacker (charPos target) =
     do announce hs $ ServerCommand targetId Stun
        return $ stunnedCharacter target
  | otherwise = return target

attackPlayer :: Handles -> Player -> Int -> Player -> IO Player
attackPlayer hs attacker charName target
  | canHitPoint attackerChar (charPos targetChar) =
     do let attackerName = playerUsername attacker
        announce hs $ ServerCommand charName Die
        announceOne hs (ConnectionId charName)
          $ ServerMessage $ "Killed by " ++ attackerName
        return $ mapPlayerCharacter deadCharacter target
  | otherwise = return target
  where
  targetChar = playerCharacter target
  attackerChar = playerCharacter attacker

mapWithKeyA :: Applicative f => (Int -> a -> f b) -> IntMap a -> f (IntMap b)
mapWithKeyA f m = sequenceA $ IntMap.mapWithKey f m

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
