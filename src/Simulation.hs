{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Data.List  (findIndex)
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random (randomRIO)

import Character
import Parameters
import VectorUtils

data Player   = Player
  { playerCharacter :: Character
  , playerUsername :: String
  , playerScore    :: Int
  , playerVisited  :: [Int]
  , playerSmokes   :: Int
  }
  deriving (Show, Read, Eq)

data ServerWorld = ServerWorld
  { serverNpcs    :: [Character]
  , serverPlayers :: [Player]
  , serverMode    :: ServerMode
  , serverLobby   :: [(Int,String)]
  }

data ServerMode = Playing | Starting Float | Stopped
  deriving (Eq, Read, Show)

-- | Lift a function on Characters to one on Players
mapPlayerCharacter :: (Character -> Character) -> Player -> Player
mapPlayerCharacter f p = p { playerCharacter = f (playerCharacter p) }

canHitPoint :: Character -> Point -> Bool
canHitPoint char pt = inRange && inFront
  where
  inRange = attackLen <= attackDistance

  -- u·v = ❘v❘ ❘u❘ cos Θ
  inFront = cos attackAngle * attackLen
         <= charFacing char `dotV` attackVector

  attackVector   = subPt pt (charPos char)
  attackLen      = magV attackVector

-- | Compute a random point inside a box.
randomPoint :: Point -> Point -> IO Point
randomPoint (minX,minY) (maxX,maxY) =
  do x <- randomRIO (minX,maxX)
     y <- randomRIO (minY,maxY)
     return (x,y)

-- | Compute a random point inside the board.
randomBoardPoint :: IO Point
randomBoardPoint = randomPoint boardMin boardMax

-- | Compute a random unit vector.
randomUnitVector :: IO Vector
randomUnitVector =
  do degrees <- randomRIO (0,359)
     let rads = degToRad $ fromInteger degrees
     return $ unitVectorAtAngle rads

-- | Construct a new character given a name, a position,
-- and a facing unit vector. When think is True,
-- the character will be scheduled to begin walking
-- after a random duration.
initServerCharacter :: Bool -> Int -> IO Character
initServerCharacter think charName =
  do charPos     <- randomBoardPoint
     charFacing  <- randomUnitVector
     waitWaiting <- pickWaitTime think
     let waitStunned = False
         charState = Waiting Wait { .. }
     return Character { .. }

-- | Construct a new player given an initial number
-- of smokebombs, an identifier, a username, and a
-- starting score.
initPlayer :: Int -> Int -> String -> Int -> IO Player
initPlayer smokes name playerUsername playerScore =
  do playerCharacter <- initServerCharacter False name
     let playerVisited = []
         playerSmokes  = smokes
     return Player { .. }

-- | When True, compute a new random wait time value.
pickWaitTime ::
  Bool {- ^ compute random delay -} ->
  IO (Maybe Float)
pickWaitTime False = return Nothing
pickWaitTime True  = fmap Just $ randomRIO (0, restTime)

-- | Determine if a point lies within a given pillar.
isInPillar ::
  Point {- ^ location to test -} ->
  Point {- ^ center of pillar -} ->
  Bool
isInPillar p (x,y) = pointInBox p (x-halfside, y-halfside)
                                  (x+halfside, y+halfside)
  where
  halfside = pillarSize / 2

-- | Determine the index, if any, of the pillar with contains a point.
whichPillar :: Point -> Maybe Int
whichPillar p = findIndex (isInPillar p) pillars

-- | Return true if the given player can use smokebombs.
hasSmokebombs :: Player -> Bool
hasSmokebombs p = playerSmokes p > 0

-- | Update a player to have one fewer smokebombs.
consumeSmokebomb :: Player -> Player
consumeSmokebomb p = p { playerSmokes = playerSmokes p - 1 }
