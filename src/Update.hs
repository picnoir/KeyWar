module Update (
  updateWorld
) where

import Graphics.Gloss.Data.Picture (Picture(..))

import World  (World)
import Consts (velocity, screenWidth)

updateWorld :: Float -> World -> IO World
updateWorld _ (Pictures pics) = return . Pictures $ fmap moveToRight activePics
  where
    activePics = filter filterOutOfBound pics

moveToRight :: Picture -> Picture
moveToRight (Translate x y p) = Translate (x - velocity) y p

filterOutOfBound :: Picture -> Bool
filterOutOfBound (Translate x _ _) = x > (- screenWidth / 2)
