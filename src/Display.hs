module Display (
  displayWorld
) where

import Graphics.Gloss.Data.Picture (Picture(..))

import World (World(..), Box(..))


displayWorld :: World -> IO Picture
displayWorld w = return $ Pictures pics 
  where
    pics = fmap picture (boxes w)
