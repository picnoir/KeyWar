module World 
( World(..),
  Box(..),
  createWorld,
  createBox) where

import           Data.Char                   (toLower)
import           Data.StateVar               (($=))
import qualified Data.HashMap.Strict as HM   (HashMap, lookup, (!))
import           Data.Maybe                  (fromMaybe)
import           Graphics.Gloss.Data.Picture (Picture(..))
import           GHC.Float                   (float2Double)
import qualified Physics.Hipmunk        as H (Body, newBody,
                                              newShape, ShapeType(..),
                                              Vector(..), Space,
                                              newSpace,
                                              Shape, position, 
                                              applyImpulse, gravity,
                                              infinity, spaceAdd) 
import           System.Random               (randomR, newStdGen)
import           Consts                      (screenWidth, screenHeight,
                                              spritesHM)



type Sprites = HM.HashMap String Picture

data World = World {
  boxes :: [Box],
  space :: !H.Space,
  sprites :: Sprites
}

data Box = Box {
  picture :: !Picture,
  body    :: !H.Body,
  shape   :: !H.Shape
}

createWorld :: IO World
createWorld = do
  s <- H.newSpace
  createWalls s
  H.gravity s $= ng
  return $ World [] s spritesHM
  where
    ng = H.Vector 0 (-500)

createBox :: String -> Sprites -> IO Box
createBox t s = do
  magnificentBody <- H.newBody 40 5
  H.position magnificentBody $= H.Vector (float2Double $ screenWidth / 2) (float2Double $ screenHeight / 3) 
  gen <- newStdGen
  let yImp = randomR (1000, 10000) gen
  H.applyImpulse magnificentBody (H.Vector (-18000) (fst yImp)) (H.Vector 0 0)
  magShape <- H.newShape magnificentBody (H.Polygon boxVertices) (H.Vector 0 0)
  return $ Box boxPic magnificentBody magShape
  where
    boxVertices   = [H.Vector (-boxWidthD / 2) (boxHeightD / 2), H.Vector (boxWidthD / 2) (boxHeightD / 2), H.Vector (boxWidthD / 2) (-boxHeightD / 2), H.Vector (-boxWidthD / 2) (-boxHeightD / 2)]
    boxPic        = Translate screenWidth 0 $ Scale 0.5 0.5 boxTex
    boxTex        = fromMaybe (s HM.! "?") $  HM.lookup (fmap toLower t) s
    boxHeightD    = float2Double boxHeight
    boxWidthD     = float2Double boxWidth
    boxHeight     = 50
    boxWidth      = 50

createWalls :: H.Space -> IO ()
createWalls s = do
  bBody <- H.newBody 0 H.infinity
  bShape <- H.newShape bBody bPath (H.Vector 0 0)
  H.spaceAdd s bBody
  H.spaceAdd s bShape
  where
    bPath = H.LineSegment (H.Vector (-sScreenWidth / 2) (-sScreenHeight / 2)) (H.Vector (-sScreenWidth / 2) (-sScreenHeight / 2)) 1
    sScreenWidth = float2Double screenWidth
    sScreenHeight = float2Double screenHeight
