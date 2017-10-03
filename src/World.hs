module World 
( World,
  initWorld,
  createBox) where

import qualified Data.Text as T              (Text, unpack, length) 
import           Graphics.Gloss.Data.Picture (Picture(..))
import           Graphics.Gloss.Data.Color   (makeColor)

import Consts                                (fontScale, screenWidth)

type World = Picture

createBox :: T.Text -> Picture
createBox t = Translate (screenWidth / 2) 0 $ Pictures [boxGeo, boxText]
  where
    boxText = Translate (-8) (-25) $ Scale fontScale fontScale $ Color (makeColor 0 0 0 255) $ Text $ T.unpack t
    boxGeo  = Scale (fontScale * 2) (fontScale * 2) $ Color (makeColor 255 255 255 255) $ Polygon [(-boxWidth,boxHeight),(boxWidth,boxHeight),(boxWidth,-boxHeight),(-boxWidth,-boxHeight)] 
    boxHeight = 35.0
    boxWidth = 20 + 15.0 * fromIntegral (T.length t)

initWorld :: World
initWorld = Pictures []
