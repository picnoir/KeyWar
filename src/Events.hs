module Events (
  handleEvents
) where

import World (World(..), Box(..),
              createBox)

import qualified Data.Text as T         (singleton)
import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..),
                                         Key(..))
import qualified Physics.Hipmunk as H   (spaceAdd)

handleEvents :: Event -> World -> IO World
handleEvents (EventKey (Char c) Down _ _ ) (World bxs s) = do
  box <- createBox $ T.singleton c
  H.spaceAdd s $ body box
  return $ World (box:bxs) s
handleEvents _ w = return w
