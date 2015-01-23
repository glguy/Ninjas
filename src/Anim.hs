{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Anim where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap (loadBMP)
import System.FilePath
import Data.Data
import Control.Exception
import Prelude


defaultFrameDelay :: Float
defaultFrameDelay = 0.2

-- | This is an animation loop.
data Animation = Animation
  { frameDelay  :: Float      -- ^ How long to wait between frames
  , moreFrames  :: [Picture]  -- ^ Total number of frames
  , waitFor     :: Float      -- ^ Time until next frame
  , curFrame    :: Picture    -- ^ Current frame
  } deriving (Data, Typeable)

loop :: Animation -> Animation
loop a = a { moreFrames = cycle' (moreFrames a) }
  where
  cycle' [] = error "Animations files missing, try 'cabal install'"
  cycle' xs = cycle xs

once :: Float -> [Picture] -> Animation
once frameDelay moreFrames = Animation { .. }
  where waitFor   = 0
        curFrame  = blank


finished :: Float -> Animation -> Bool
finished e a = null (moreFrames a) && waitFor a <= e

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
loadImg x = loadBMP ("images" </> x <.> "bmp")

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
        deriving (Data, Typeable)

loadNPC :: IO NPC
loadNPC =
  do walk   <- loop `fmap` loadAnim "walk"
     stay   <- loadAnim "stay"
     stun   <- loadAnim "stunned"
     attack <- loop `fmap` loadAnim "attack"
     die    <- loadAnim "death"
     return NPC { .. }


data World = World { background, tower, smoke :: Animation
                   , npc :: NPC }
        deriving (Data, Typeable)

loadWorld :: IO World
loadWorld =
  do npc <- loadNPC
     tower <- loadAnim "tower"
     smoke <- loadSmoke
     let background = once defaultFrameDelay []
     return World { .. }

loadSmoke :: IO Animation
loadSmoke =
  do a <- loadAnim "smoke"
     let fs  = case moreFrames a of
                 [] -> [blank]
                 xs -> xs
         allFs = fs ++ tail (reverse fs)
     let n = fromIntegral (length allFs) :: Float
     return a { moreFrames = allFs, frameDelay = smokeLen / n }
  where
  smokeLen = 5 -- secs


updateWorld :: Float -> World -> World
updateWorld e w = w { background = update e (background w)
                    , tower      = update e (tower w)
                    }



