module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Data.Color        (makeColor)
import Graphics.Gloss.Data.Display      (Display(..))
import qualified Physics.Hipmunk as H


import World   (createWorld)
import Display (displayWorld)
import Events  (handleEvents)
import Update  (updateWorld)
import Consts  (screenWidth, screenHeight)

main :: IO ()
main = do
      initWorld <- createWorld
      H.initChipmunk
      playIO
        (InWindow "KeyWar" (floor screenWidth,floor screenHeight) (0,0))
        (makeColor 0 255 0 255)
        60
        initWorld
        displayWorld
        handleEvents
        updateWorld
