module Update (
  updateWorld
) where

import Control.Monad                  (filterM, when)
import Graphics.Gloss.Data.Picture    (Picture(..))
import qualified Physics.Hipmunk as H (spaceRemove)

import World  (World(..), Box(..))
import Consts (velocity, screenWidth)

updateWorld :: Float -> World -> IO World
updateWorld _ (World bxs s) = do
  activePics <- filterM filterOutOfBound bxs
  return $ World (fmap moveToRight activePics) s 
  where
    filterOutOfBound b@(Box (Translate x _ _) _) = do
      let shouldRemove = x > (- screenWidth / 2)
      when shouldRemove $ H.spaceRemove s $ body b
      return shouldRemove

moveToRight :: Box -> Box
moveToRight (Box (Translate x y p) b) = Box newPic b
  where
    newPic = Translate (x - velocity) y p
