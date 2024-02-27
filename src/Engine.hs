
module Engine
  ( initConf, Conf, run
  ) where

import Control.Concurrent (threadDelay)
import Foreign.C.Types (CInt)
import GHC.Word (Word8)
import Linear.V4 (V4(..))
import Pic (Pic,V2(..))
import SDL (Renderer,($=))
import Wad (Wad,Vertex,Int16)
import qualified Data.Text as Text (pack)
import qualified DrawMap (draw)
import qualified Pic (Pic(..))
import qualified SDL

data Conf = Conf
  { resX :: Int16
  , resY :: Int16
  , sf :: CInt
  , offset :: V2 Float
  , scale :: V2 Float
  } deriving Show

type BB = (Vertex,Vertex)

initConf :: BB -> Conf
initConf bb = Conf
  { resX, resY, sf
  , offset = V2 offsetX offsetY
  , scale = V2 scaleX scaleY
  }
  where
    sf = 4
    border = 0
    resX = 320
    resY = 200
    (V2 minX minY,V2 maxX maxY) = bb
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
  setColor r white -- in case we forget to set when rendering Pic
  renderPic conf assets pic
  SDL.present r
  where
    darkGrey = rgb (50,50,50)
    white = rgb (255,255,255)

renderPic :: Conf -> DrawAssets -> Pic () -> IO ()
renderPic Conf{sf,offset,scale} DrawAssets{renderer=r} = loop
  where
    yellow = rgb (255,255,0) -- TODO: colours should come with Dot/Line
    red = rgb (255,0,0) -- TODO: colours should come with Dot/Line

    remap :: V2 Float -> V2 Float
    remap p = (p + offset) * scale

    quantize :: Float -> CInt
    quantize a = sf * floor a

    loop :: Pic a -> IO a
    loop pic = case pic of
      Pic.Ret a -> pure a
      Pic.Bind m f -> do b <- loop m; loop (f b)
      Pic.Dot p -> do
        let p' = fmap quantize (remap p)
        let rect = SDL.Rectangle (SDL.P p') (V2 sf sf) -- TODO: circle would be nice
        setColor r yellow
        SDL.fillRect r (Just rect)

      Pic.Line a b -> do
        let a' = fmap quantize (remap a)
        let b' = fmap quantize (remap b)
        setColor r red
        SDL.drawLine r (SDL.P a') (SDL.P b')


type RGB = (Word8,Word8,Word8)
type Colour = V4 Word8

rgb :: RGB -> Colour
rgb (r,g,b) = V4 r g b 255

setColor :: SDL.Renderer -> Colour -> IO ()
setColor r c = SDL.rendererDrawColor r $= c
