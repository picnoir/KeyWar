module Update (
  updateWorld
) where

import Data.Maybe                     (maybeToList)
import Control.Monad                  (filterM, unless)
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TChan   (tryReadTChan)
import Data.StateVar                  (get)
import GHC.Float                      (double2Float, float2Double)
import Graphics.Gloss.Data.Picture    (Picture(..))
import qualified Physics.Hipmunk as H (spaceRemove, position,
                                       Vector(..), step, spaceAdd,
                                       angle)

import World                           (World(..), Box(..), createBox)
import Consts                          (screenWidth)

updateWorld :: Float -> World -> IO World
updateWorld dt (World bxs s sprts evtsChan e) = do
  nEvent <- atomically $ tryReadTChan evtsChan
  let boxText = handleEnabled <$> nEvent
  nBoxMb <- mapM (createBox e sprts) boxText
  let nEnabled = maybe e toggleEnabled nEvent
      nBox     = maybeToList nBoxMb
  unless (null nBox) $ do
     H.spaceAdd s . body $ head nBox
     H.spaceAdd s . shape $ head nBox
  H.step s $ float2Double dt
  activePics <- filterM filterOutOfBound bxs
  nbxs       <- mapM moveBox activePics
  return $ World (nBox ++ nbxs) s sprts evtsChan nEnabled
  where
    filterOutOfBound b@(Box (Translate x _ _) _ _) = do
      let shouldRemove = x > (- screenWidth / 2)
      unless shouldRemove . H.spaceRemove s $ body b
      unless shouldRemove . H.spaceRemove s $ shape b
      return shouldRemove
    toggleEnabled val = if val == "<Menu>"
                          then not e
                          else e
    handleEnabled txt = if e
                          then txt
                          else "?"

moveBox :: Box -> IO Box
moveBox (Box (Translate _ _ (Rotate _ p)) b sh) = do
  pos <- get $ H.position b
  rot <- get $ H.angle b
  let newPic = Translate (getX pos) (getY pos) $ Rotate (double2Float $  newAngle rot) p
  return $ Box newPic b sh
  where
    newAngle pA = (-pA) * 180 / 3.14
    getX (H.Vector x _ ) = double2Float x
    getY (H.Vector _ y ) = double2Float y
