module VectorUtils where

import Graphics.Gloss.Data.Vector

-- | Add two vectors
addPt :: Vector -> Vector -> Vector
addPt (x,y) (a,b) = (x+a,y+b)

-- | Subtract two vectors
subPt :: Vector -> Vector -> Vector
subPt (x,y) (a,b) = (x-a,y-b)

