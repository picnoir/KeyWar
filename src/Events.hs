module Events (
  handleEvents
) where

import World (World, createBox)

import qualified Data.Text as T         (singleton)
import Graphics.Gloss.Data.Picture      (Picture(..))
import Graphics.Gloss.Interface.IO.Game (Event(..), KeyState(..),
                                         Key(..))

handleEvents :: Event -> World -> IO World
handleEvents (EventKey (Char c) Down _ _ ) (Pictures pics) = return . Pictures $ box:pics
  where box = createBox $ T.singleton c
handleEvents _ w = return w
