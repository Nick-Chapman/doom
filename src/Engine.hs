
module Engine
  ( initConf, Conf, run
  ) where

import Control.Concurrent (threadDelay)
import Foreign.C.Types (CInt)
import GHC.Word (Word8)
import Pic (Pic,Point)
import SDL (Renderer,V2(..),V4(..),($=))
import Wad (Wad,Vertex,Int16)
import qualified Data.Text as Text (pack)
import qualified DrawMap (draw)
import qualified Pic (Pic(..))
import qualified SDL

data Conf = Conf
  { resX :: Int16
  , resY :: Int16
  , sf :: CInt
  , offset :: Point
  , scale :: Point
  } deriving Show

type BB = (Vertex,Vertex)

initConf :: BB -> Conf
initConf bb = Conf { resX, resY, sf, offset = (offsetX,offsetY), scale = (scaleX,scaleY) }
  where
    sf = 4
    border = 0
    resX = 320
    resY = 200
    ((minX,minY),(maxX,maxY)) = bb
    sizeX = maxX - minX + 1
    sizeY = maxY - minY + 1
    scaleX = fromIntegral (resX - 2*border) / fromIntegral sizeX
    scaleY = fromIntegral (resY - 2*border) / fromIntegral sizeY
    offsetX = fromIntegral (-minX) + fromIntegral border/scaleX
    offsetY =  fromIntegral (-minY) + fromIntegral border/scaleY

windowSize :: Conf -> V2 CInt
windowSize Conf{sf,resX,resY} = V2 (sf * fromIntegral resX) (sf * fromIntegral resY)

run :: Conf -> Wad -> IO ()
run conf wad = do
  SDL.initializeAll
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize conf }
  win <- SDL.createWindow (Text.pack "Wolf") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  let assets = DrawAssets { win, renderer }
  let
    loop :: Int -> IO ()
    loop n = do
      --print ("frame",n)
      --TODO: get events; update state
      --SDL.windowSize win $= windowSize conf -- resize
      let pic = DrawMap.draw wad
      drawEverything conf assets pic
      threadDelay 1000000 -- 1sec
      loop (n+1)
  loop 0
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

data DrawAssets = DrawAssets
  { renderer :: Renderer
  , win :: SDL.Window
  }

drawEverything :: Conf -> DrawAssets -> Pic () -> IO ()
drawEverything conf assets@DrawAssets{renderer=r} pic = do
  setColor r darkGrey
  SDL.clear r
  renderPic conf assets pic
  SDL.present r
  where
    darkGrey = rgb (20,20,20)

renderPic :: Conf -> DrawAssets -> Pic () -> IO ()
renderPic Conf{sf,offset,scale} DrawAssets{renderer=r} = loop
  where
    yellow = rgb (255,255,0)

    remap :: Point -> Point
    remap p = (p `add` offset) `mul` scale

    loop :: Pic a -> IO a
    loop pic = case pic of
      Pic.Ret a -> pure a
      Pic.Bind m f -> do b <- loop m; loop (f b)
      Pic.Dot p -> do
        let p' = remap p
        let (x0,y0) = p'
        setColor r yellow
        let x = sf * floor x0
        let y = sf * floor y0
        let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
        SDL.fillRect r (Just rect)

type RGB = (Word8,Word8,Word8)
type Colour = V4 Word8

rgb :: RGB -> Colour
rgb (r,g,b) = V4 r g b 255

setColor :: SDL.Renderer -> Colour -> IO ()
setColor r c = SDL.rendererDrawColor r $= c

add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

mul :: Point -> Point -> Point
mul (x1,y1) (x2,y2) = (x1*x2,y1*y2)
