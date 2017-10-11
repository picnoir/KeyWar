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
import           GHC.Float                   (float2Double, double2Float)
import qualified Physics.Hipmunk        as H (Body, newBody,
                                              newShape, ShapeType(..),
                                              Vector(..), Space,
                                              newSpace,
                                              Shape, position, 
                                              applyImpulse, gravity,
                                              infinity, spaceAdd,
                                              friction, elasticity,
                                              StaticShape(..))
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
  f <- createWalls s
  H.gravity s $= ng
  return $ World [f] s spritesHM evtsChan True
  where
    ng = H.Vector 0 (-500)

createBox :: Sprites -> String -> IO Box
createBox s t = do
  magnificentBody <- H.newBody 40 5
  gen <- newStdGen
  let yImp = randomR (1000, 10000) gen
  H.applyImpulse magnificentBody (H.Vector (-18000) (fst yImp)) (H.Vector 0 0)
  let dy = randomR (0, 100) gen
  H.position magnificentBody $= H.Vector (float2Double $ screenWidth / 2) (float2Double $ screenHeight / 5 + (fst dy)) 
  magShape <- H.newShape magnificentBody (H.Polygon boxVertices) (H.Vector 0 0)
  H.friction magShape $= 1
  H.elasticity magShape $= 1 
  return $ Box boxPic magnificentBody magShape
  where
    boxVertices   = [H.Vector (-boxWidthD / 2) (boxHeightD / 2), H.Vector (boxWidthD / 2) (boxHeightD / 2), H.Vector (boxWidthD / 2) (-boxHeightD / 2), H.Vector (-boxWidthD / 2) (-boxHeightD / 2)]
    boxPic        = Translate screenWidth 0 $ Scale 0.5 0.5 boxTex
    boxTex        = fromMaybe (s HM.! "?") $  HM.lookup (toLower <$> logKeysToSpriteTransco t) s
    boxHeightD    = float2Double boxHeight
    boxWidthD     = float2Double boxWidth
    boxHeight     = 50
    boxWidth      = 50

createWalls :: H.Space -> IO Box 
createWalls s = do
  bBody <- H.newBody 0 0
  bShape <- H.newShape bBody bPath (H.Vector 0 0)
  H.position bBody $= H.Vector 0 0
  H.friction bShape $= H.infinity
  H.elasticity bShape $= 1
  H.spaceAdd s (H.Static bShape)
  return $ Box (Translate 0 0 $ Line path) bBody bShape
  where
    bPath         = H.LineSegment (H.Vector (-sScreenWidth / 2) (-sScreenHeight / 2)) (H.Vector (-sScreenWidth / 2) (-sScreenHeight / 2)) 1
    sScreenWidth  = float2Double screenWidth
    sScreenHeight = float2Double screenHeight
    fScreenWidth  = double2Float sScreenWidth
    path          = [(0, 0), (fScreenWidth/2, 0)]

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
