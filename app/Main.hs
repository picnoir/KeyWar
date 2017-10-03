module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Data.Color        (makeColor)
import Graphics.Gloss.Data.Display      (Display(..))


import World   (initWorld)
import Display (displayWorld)
import Events  (handleEvents)
import Update  (updateWorld)
import Consts  (screenWidth, screenHeight)

main :: IO ()
main = playIO
        (InWindow "KeyWar" (floor screenWidth,floor screenHeight) (10,10))
        (makeColor 0 255 0 255)
        60
        initWorld
        displayWorld
        handleEvents
        updateWorld
