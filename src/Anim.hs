module Anim where

import Graphics.Gloss.Data.Picture
import System.FilePath
import Paths_Ninjas


data AnimData = AnimData
  { walkFrames :: [Picture]
  , stayFrame  :: Picture
  , attackFrame :: Picture
  , stunnedFrame :: Picture
  }

loadImg :: FilePath -> IO Picture
loadImg x = loadBMP =<< getDataFileName ("images" </> x)

loadAnimData :: IO AnimData
loadAnimData =
  do p1 <- loadImg "walk1.bmp"
     p2 <- loadImg "walk2.bmp"
     p3 <- loadImg "walk3.bmp"
     a1 <- loadImg "attack.bmp"
     s1 <- loadImg "stunned.bmp"
     return AnimData { walkFrames = [p1,p2,p1,p3]
                     , stayFrame  = p1
                     , attackFrame = a1
                     , stunnedFrame = s1
                     }





