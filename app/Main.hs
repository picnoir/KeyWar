module Main where

import Control.Concurrent.STM                (atomically)
import Control.Concurrent.STM.TChan          (TChan, newTChanIO, writeTChan)
import Control.Concurrent                    (forkIO, killThread)
import qualified Data.ByteString       as BS (ByteString)
import qualified Data.ByteString.Char8 as BC (unpack)
import Data.Maybe                            (fromMaybe)
import Graphics.Gloss.Interface.IO.Game      (playIO)
import Graphics.Gloss.Data.Color             (makeColor)
import Graphics.Gloss.Data.Display           (Display(..))
import qualified Physics.Hipmunk        as H (initChipmunk)
import System.Console.ParseArgs              (parseArgsIO, ArgsParseControl(..),
                                              ArgsComplete(..), ArgsDash(..),
                                              Arg(..), Argtype(..), argDataDefaulted,
                                              getArg, Args)
import System.IO.TailFile                    (tailFile)


import World                                 (createWorld)
import Display                               (displayWorld)
import Events                                (handleEvents)
import Update                                (updateWorld)
import Consts                                (screenWidth, screenHeight)

main :: IO ()
main = do
  evtsChan    <- newTChanIO
  logFilePath <- getLoggerFile
  threadId    <- forkIO $ inputReaderThread logFilePath evtsChan
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

inputReaderThread :: FilePath -> TChan String ->  IO ()
inputReaderThread logFile evtsChan = tailFile logFile update $ return evtsChan

update :: TChan String -> BS.ByteString -> IO (TChan String)
update evtsChan input = do
  atomically . writeTChan evtsChan $ BC.unpack input
  return evtsChan

getLoggerFile :: IO FilePath
getLoggerFile = do
  let apc = ArgsParseControl ArgsComplete ArgsHardDash
  let args = [Arg 0 (Just 'l') (Just "logFile") 
              (argDataDefaulted "logFile" ArgtypeString "/tmp/keylogger.log")
                "Logger output file."]
  rArgs <- parseArgsIO apc args :: IO (Args Integer)
  return . fromMaybe "/tmp/keylogger.log" $ getArg rArgs 0
