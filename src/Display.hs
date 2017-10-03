module Display (
  displayWorld
) where

import Graphics.Gloss.Data.Picture (Picture(..))

import World (World)


displayWorld :: World -> IO Picture
displayWorld = return 

