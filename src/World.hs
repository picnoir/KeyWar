module World 
( World(..),
  Box(..),
  createWorld,
  createBox) where

import           Control.Concurrent.STM.TChan(TChan)
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
                                              infinity, spaceAdd,
                                              friction, elasticity,
                                              momentForPoly, StaticShape(..))
import           System.Random               (randomR, newStdGen)
import           Consts                      (screenWidth, screenHeight,
                                              spritesHM)



type Sprites = HM.HashMap String Picture

data World = World {
  boxes   :: [Box],
  space   :: !H.Space,
  sprites :: !Sprites,
  chan    :: !(TChan String),
  enabled :: !Bool
}

data Box = Box {
  picture :: !Picture,
  body    :: !H.Body,
  shape   :: !H.Shape
}

createWorld :: TChan String -> IO World
createWorld evtsChan = do
  s <- H.newSpace
  createWalls s
  H.gravity s $= ng
  return $ World [] s spritesHM evtsChan True
  where
    ng = H.Vector 0 (-500)

createBox :: Bool -> Sprites -> String -> IO Box
createBox heavyBox s t = do
  gen <- newStdGen
  let yImp   = randomR (10, 100) gen
  let dy     = randomR (0, 100) gen
  let mBodyI = H.momentForPoly bMass boxVertices (H.Vector 0 0)
  magnificentBody <- H.newBody 10 mBodyI
  H.applyImpulse magnificentBody (H.Vector (-xImp) (fst yImp)) (H.Vector 0 0)
  H.position magnificentBody $= H.Vector (float2Double $ screenWidth / 2) (float2Double $ screenHeight / 5 + fst dy) 
  magShape <- H.newShape magnificentBody (H.Polygon boxVertices) (H.Vector 0 0)
  H.friction magShape $= 2
  H.elasticity magShape $= 0.5 
  return $ Box boxPic magnificentBody magShape
  where
    xImp          = if heavyBox
                      then 6500
                      else 10000
    bMass         = if heavyBox
                      then 50
                      else 10
    boxVertices   = [H.Vector (-boxWidthD / 2) (boxHeightD / 2), H.Vector (boxWidthD / 2) (boxHeightD / 2), H.Vector (boxWidthD / 2) (-boxHeightD / 2), H.Vector (-boxWidthD / 2) (-boxHeightD / 2)]
    boxPic        = Translate screenWidth 0 $ Rotate 0 $ Scale 0.5 0.5 boxTex
    boxTex        = fromMaybe (s HM.! "?") $  HM.lookup (toLower <$> logKeysToSpriteTransco t) s
    boxHeightD    = float2Double boxHeight
    boxWidthD     = float2Double boxWidth
    boxHeight     = 50
    boxWidth      = 50

createWalls :: H.Space -> IO ()
createWalls s = do
  bBody <- H.newBody H.infinity H.infinity 
  bBottomShape <- H.newShape bBody bPath (H.Vector 0 0)
  bTopShape <- H.newShape bBody bPath (H.Vector 0 sScreenHeight)
  H.position bBody $= H.Vector 0 0
  H.friction bBottomShape $= 0.1
  H.friction bTopShape $= 0.1
  H.elasticity bBottomShape $= 1 
  H.elasticity bTopShape $= 1 
  H.spaceAdd s (H.Static bTopShape)
  H.spaceAdd s (H.Static bBottomShape)
  where
    bPath         = H.LineSegment (H.Vector (-sScreenWidth / 2) (-sScreenHeight / 2)) (H.Vector (sScreenWidth / 2) (-sScreenHeight / 2)) 1
    sScreenWidth  = float2Double screenWidth
    sScreenHeight = float2Double screenHeight

logKeysToSpriteTransco :: String -> String
logKeysToSpriteTransco input =
  case input of
    "<CpsLk>"  -> "esc"
    "<LShft>"  -> "shift"
    "<RShft>"  -> "shift"
    "<LCtrl>"  -> "ctrl"
    "<RCtrl>"  -> "ctrl"
    "<LAlt>"   -> "alt"
    "<AltGr>"  -> "alt"
    "<LMeta>"  -> "super"
    "<RMeta>"  -> "super"
    "<BckSp>"  -> "backspace"
    "<Left>"   -> "left"
    "<Right>"  -> "right"
    "<Down>"   -> "down"
    "<Up>"     -> "up"
    "<Tab>"    -> "tab"
    " "        -> "space"
    "\n"       -> "enter"
    "\\"       -> "aslash"
    "/"        -> "slash"
    _          -> input
