module Main where

import Control.Concurrent.STM                (atomically)
import Control.Concurrent.STM.TChan          (TChan, newTChanIO, writeTChan)
import Control.Concurrent                    (forkIO, killThread)
import qualified Data.ByteString as BS       (ByteString)
import qualified Data.ByteString.Char8 as BC (unpack)
import Graphics.Gloss.Interface.IO.Game      (playIO)
import Graphics.Gloss.Data.Color             (makeColor)
import Graphics.Gloss.Data.Display           (Display(..))
import qualified Physics.Hipmunk as H
import System.IO.TailFile                    (tailFile)


import World   (createWorld)
import Display (displayWorld)
import Events  (handleEvents)
import Update  (updateWorld)
import Consts  (screenWidth, screenHeight)

main :: IO ()
main = do
  evtsChan <- newTChanIO
  threadId <- forkIO $ inputReaderThread evtsChan
  graphicsThread evtsChan
  killThread threadId
  return ()

graphicsThread :: TChan String -> IO ()
graphicsThread evtsChan = do
      initWorld <- createWorld evtsChan
      H.initChipmunk
      playIO
        (InWindow "KeyWar" (floor screenWidth,floor screenHeight) (0,0))
        (makeColor 0 255 0 255)
        60
        initWorld
        displayWorld
        handleEvents
        updateWorld

inputReaderThread :: TChan String ->  IO ()
inputReaderThread evtsChan = tailFile "/home/minoulefou/test.log" update $ return evtsChan

update :: TChan String -> BS.ByteString -> IO (TChan String)
update evtsChan input = do
  atomically $ writeTChan evtsChan $ BC.unpack input
  return evtsChan
