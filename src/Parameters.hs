module Parameters where

import Graphics.Gloss.Data.Point (Point)

-- | Smallest point on the board
boardMin :: Point
boardMin = (-350,-250)

-- | Largest point on the board
boardMax :: Point
boardMax = (350,250)

-- | Maximum time an NPC will wait before choosing a new destination
restTime :: Float
restTime = 2

-- | Radius of ninja sprite in pixels. This is used for drawing
-- a ninja and determining when he is hidden under smoke.
ninjaRadius :: Float
ninjaRadius = 10

-- | Maximum distance from the center of a player where an attack has an effect
attackDistance :: Float
attackDistance = 50

-- | Maximum angle in radians in either direction from a player's direction of
-- travel where the attacks have an effect
attackAngle    :: Float
attackAngle    = pi / 3

-- | The number of time update events gloss should attempt to run per second
eventsPerSecond :: Int
eventsPerSecond = 100

-- | Pixels per second traveled by ninjas
speed :: Float
speed = 100

-- | Duration in seconds a player is stunned after an attack
attackDelay :: Float
attackDelay = 1

-- | Duration in second for which an character will be stunned after an attack
stunTime :: Float
stunTime = 3

-- | The locations of the centers of the win locations in the game
pillars :: [Point]
pillars = [(0,0), (275, 175), (-275, 175), (-275, -175), (275, -175)]

-- | The length of a side of the win location squares
pillarSize :: Float
pillarSize = 40
