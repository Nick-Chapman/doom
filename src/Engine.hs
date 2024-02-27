
module Engine
  ( initConf, Conf, run
  ) where

import Control.Concurrent (threadDelay)
import Foreign.C.Types (CInt)
import Pic (Pic,V2(..),Colour,rgb)
import SDL (Renderer,($=))
import Wad (Wad,Vertex)
import qualified Data.Text as Text (pack)
import qualified DrawMap (draw)
import qualified Pic (Pic(..))
import qualified SDL

data Conf = Conf
  { resX :: CInt
  , resY :: CInt
  , border :: CInt
  , sf :: CInt
  , offset :: V2 Float
  , scale :: V2 Float
  } deriving Show

type BB = (Vertex,Vertex)

initConf :: BB -> Conf
initConf bb = Conf
  { resX, resY, border, sf
  , offset = V2 offsetX offsetY
  , scale = V2 scaleX scaleY
  }
  where
    sf = 3
    border = 10
    resX = 320
    resY = 200
    (V2 minX minY,V2 maxX maxY) = bb
    sizeX = maxX - minX + 1
    sizeY = maxY - minY + 1
    scaleX = fromIntegral (resX - 2*border) / fromIntegral sizeX
    scaleY = fromIntegral (resY - 2*border) / fromIntegral sizeY
    offsetX = fromIntegral (-minX)
    offsetY =  fromIntegral (-minY)

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
  SDL.drawRect r Nothing
  renderPic conf assets pic
  SDL.present r
  where
    darkGrey = rgb (50,50,50)
    white = rgb (255,255,255)

renderPic :: Conf -> DrawAssets -> Pic () -> IO ()
renderPic Conf{resY,sf,border,offset,scale} DrawAssets{renderer=r} = loop
  where

    remap :: V2 Float -> V2 Float
    remap p = (p + offset) * scale

    quantize :: Float -> CInt
    quantize a = (border + floor a)

    flipY (V2 x y) = V2 x (resY - y)

    snap :: V2 Float  -> V2 CInt
    snap = (V2 sf sf *) . flipY . fmap quantize . remap

    loop :: Pic a -> IO a
    loop pic = case pic of
      Pic.Ret a -> pure a
      Pic.Bind m f -> do b <- loop m; loop (f b)
      Pic.Dot col p -> do
        setColor r col
        let p' = snap p
        --SDL.drawPoint r (SDL.P p')
        SDL.drawRect r (Just (SDL.Rectangle (SDL.P p') (V2 sf sf)))
      Pic.Line col a b -> do
        setColor r col
        let a' = snap a
        let b' = snap b
        SDL.drawLine r (SDL.P a') (SDL.P b')

setColor :: SDL.Renderer -> Colour -> IO ()
setColor r c = SDL.rendererDrawColor r $= c
