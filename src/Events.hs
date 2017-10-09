module Events (
  handleEvents
) where

import World (World(..), Box(..),
              createBox)

import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..),
                                         Key(..))
import qualified Physics.Hipmunk as H   (spaceAdd)

handleEvents :: Event -> World -> IO World
handleEvents (EventKey (Char c) Down _ _ ) (World bxs s sprts) = do
  box <- createBox [c] sprts
  H.spaceAdd s $ shape box
  H.spaceAdd s $ body box
  return $ World (box:bxs) s sprts
handleEvents _ w = return w
