{-# LANGUAGE TemplateHaskell #-}

module Consts where

import Codec.BMP                           (parseBMP)
import Data.ByteString                     (ByteString)
import Data.ByteString.Lazy                (fromStrict)
import Data.FileEmbed                      (embedDir)
import Data.Either.Extra                   (fromRight)
import qualified Data.HashMap.Strict as HM (HashMap, empty,
                                            insert)
import Graphics.Gloss                      (bitmapOfBMP, Picture)                                            


screenWidth :: Float
screenWidth = 1500

screenHeight :: Float
screenHeight = 400

fontScale :: Float
fontScale = 0.5

embededSprites :: [(FilePath, ByteString)]
embededSprites = $(embedDir "./data/img/keys/")

spritesHM :: HM.HashMap String Picture 
spritesHM = foldr convertBmp HM.empty embededSprites

convertBmp :: (FilePath, ByteString) -> HM.HashMap String Picture -> HM.HashMap String Picture
convertBmp (k, bImg) hm = fromRight hm (insertBmpToHm <$> rcBmp)
  where
    rcBmp = bitmapOfBMP <$> parseBMP (fromStrict bImg)
    insertBmpToHm = flip (HM.insert $ getAssetName k) hm

getAssetName :: FilePath -> String 
getAssetName fln = takeWhile (/= '/') removeExt
  where
    removeExt = reverse $ drop 4 (reverse fln)
