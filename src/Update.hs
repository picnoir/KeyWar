module Update (
  updateWorld
) where

import Control.Monad                  (filterM, unless)
import Data.StateVar                  (get)
import GHC.Float                      (double2Float, float2Double)
import Graphics.Gloss.Data.Picture    (Picture(..))
import qualified Physics.Hipmunk as H (spaceRemove, position,
                                       Vector(..), step)
import World  (World(..), Box(..))
import Consts (screenWidth)


updateWorld :: Float -> World -> IO World
updateWorld dt (World bxs s) = do
  H.step s $ float2Double dt
  activePics <- filterM filterOutOfBound bxs
  nbxs <- mapM moveBox activePics
  return $ World nbxs s 
  where
    filterOutOfBound b@(Box (Translate x _ _) _ _) = do
      let shouldRemove = x > (- screenWidth / 2)
      unless shouldRemove . H.spaceRemove s $ body b
      unless shouldRemove . H.spaceRemove s $ shape b
      return shouldRemove

moveBox :: Box -> IO Box
moveBox (Box (Translate _ _ p) b sh) = do
  pos <- get $ H.position b
  let newPic = Translate (getX pos - 1) (getY pos) p
  return $ Box newPic b sh
  where
    getX (H.Vector x _ ) = double2Float x
    getY (H.Vector _ y ) = double2Float y
