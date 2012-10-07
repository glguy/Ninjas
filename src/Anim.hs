{-# LANGUAGE RecordWildCards #-}
module Anim where

import Graphics.Gloss.Data.Picture
import System.FilePath
import Control.Exception
import Paths_Ninjas
import Prelude hiding(catch)


defaultFrameDelay :: Float
defaultFrameDelay = 0.2

-- | This is an animation loop.
data Animation = Animation
  { frameDelay  :: Float      -- ^ How long to wait between frames
  , moreFrames  :: [Picture]  -- ^ Total number of frames
  , waitFor     :: Float      -- ^ Time until next frame
  , curFrame    :: Picture    -- ^ Current frame
  }

loop :: Animation -> Animation
loop a = a { moreFrames = cycle (moreFrames a) }

once :: Float -> [Picture] -> Animation
once frameDelay moreFrames = Animation { .. }
  where waitFor   = 0
        curFrame  = blank

update :: Float -> Animation -> Animation
update elapsed a
  | elapsed > waitFor a =
      a { waitFor     = frameDelay a
        , curFrame    = nextFrame
        , moreFrames  = nextFrames
        }
  | otherwise = a { waitFor = waitFor a - elapsed }

  where (nextFrame,nextFrames) = case moreFrames a of
                                   []     -> (curFrame a, [])
                                   f : fs -> (f, fs)



--------------------------------------------------------------------------------

loadImg :: FilePath -> IO Picture
loadImg x = loadBMP =<< getDataFileName ("images" </> x <.> "bmp")

loadFrames :: FilePath -> IO [Picture]
loadFrames path = load (1::Int)
  where
  load n = do let i = path ++ show n
              p <- loadImg i
              ps <- load (n+1)
              return (p:ps)
            `catch` \SomeException {} -> return []

loadAnim :: FilePath -> IO Animation
loadAnim path = once defaultFrameDelay `fmap` loadFrames path



data NPC = NPC { walk, stay, stun, attack, die :: Animation }

loadNPC :: IO NPC
loadNPC =
  do walk   <- loop `fmap` loadAnim "walk"
     stay   <- loadAnim "stay"
     stun   <- loadAnim "stunned"
     attack <- loop `fmap` loadAnim "attack"
     die    <- loadAnim "death"
     return NPC { .. }


data World = World { background, tower :: Animation
                   , npc :: NPC }

loadWorld :: IO World
loadWorld =
  do npc <- loadNPC
     tower <- loadAnim "tower"
     let background = once defaultFrameDelay []
     return World { .. }


updateWorld :: Float -> World -> World
updateWorld e w = w { background = update e (background w)
                    , tower      = update e (tower w)
                    }



