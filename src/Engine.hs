
module Engine
  ( initConf, Conf, run
  ) where

import Control.Concurrent (threadDelay)
import DrawMap (Views(..))
import Foreign.C.Types (CInt)
import Colour(Colour,darkGrey,white)
import Pic (Pic,V2(..))
import ProjectToScreen (POV,getPOV,turnL,turnR)
import SDL (Renderer,($=),InputMotion(..))
import SDL.Input.Keyboard.Codes -- (???)
import Wad (Wad(..),Vertex)
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
  , randCols :: [Colour]
  } deriving Show

type VV = (Vertex,Vertex)

initConf :: VV -> [Colour] -> Conf
initConf bb randCols = Conf
  { resX, resY, border, sf
  , offset = V2 offsetX offsetY
  , scale = V2 scaleX scaleY
  , randCols = cycle randCols
  }
  where
    sf = 4
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
  let Conf{randCols} = conf
  SDL.initializeAll
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize conf }
  win <- SDL.createWindow (Text.pack "Doom") $ winConfig
  renderer <- SDL.createRenderer win (-1)
    SDL.defaultRenderer { SDL.rendererType = SDL.UnacceleratedRenderer }
  let assets = DrawAssets { win, renderer }
  let fps = 10
  let del :: Int = 1000000 `div` fps -- rough and ready
  let
    loop :: Int -> State -> IO ()
    loop n state = do
      --print ("frame",n,state)
      --SDL.windowSize win $= windowSize conf -- resize
      let State{pov,views} = state
      let pic = DrawMap.draw views randCols wad pov
      drawEverything conf assets pic
      threadDelay del
      events <- SDL.pollEvents
      processEvents state events >>= \case
        Nothing -> pure () -- Quit
        Just state -> loop (n+1) state
  loop 0 (initState wad)
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

data State = State
  { pov :: POV
  --, segI :: Int
  , views :: Views
  } deriving Show

initState :: Wad -> State
initState wad = do
  let Wad{player} = wad
  let pov = getPOV player
  State
    { pov
    --, segI = 0
    , views = View3
    }

processEvents :: State -> [SDL.Event] -> IO (Maybe State)
processEvents state = \case
  [] -> pure (Just state)
  e1:es -> do
    processEvent state e1 >>= \case
      Nothing -> pure Nothing -- Quit
      Just state -> processEvents state es

processEvent :: State -> SDL.Event -> IO (Maybe State)
processEvent state = \case
  SDL.Event _ SDL.QuitEvent -> pure Nothing -- Quit
  SDL.Event _ (SDL.KeyboardEvent ke) -> do
    let key = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
    let motion = SDL.keyboardEventKeyMotion ke
    let State{pov,views} = state
    case (key,motion) of
      (KeycodeEscape,Pressed) -> pure Nothing
      -- (KeycodeReturn,Pressed) -> pure (Just state { segI = 1 + segI })
      (KeycodeReturn,Pressed) -> pure (Just state { views = cycleViews views })
      (KeycodeLeft,Pressed) -> pure (Just state { pov = turnL pov })
      (KeycodeRight,Pressed) -> pure (Just state { pov = turnR pov })
      _ -> pure (Just state)
  SDL.Event _ _ -> pure (Just state)

cycleViews :: Views -> Views
cycleViews = \case
  View3 -> ViewBoth
  ViewBoth -> View2
  View2 -> View3

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
      Pic.Pause -> do
        SDL.present r
        threadDelay 100000
      Pic.Dot col p -> do
        setColor r col
        let p' = snap p
        SDL.drawRect r (Just (SDL.Rectangle (SDL.P p') (V2 sf sf)))
      Pic.Line col a b -> do
        setColor r col
        let a' = snap a
        let b' = snap b
        SDL.drawLine r (SDL.P a') (SDL.P b')
      Pic.Rect col p q -> do
        setColor r col
        let p' = snap p
        let q' = snap q
        SDL.drawRect r (Just (SDL.Rectangle (SDL.P p') (q' - p')))
      Pic.Mes s -> do
        putStrLn ("Mes:"++s)
      Pic.Eff io -> io

      Pic.LineQ col p q -> do
        setColor r col
        let p' = (V2 sf sf *) $ flipY (fmap fromIntegral p)
        let q' = (V2 sf sf *) $ flipY (fmap fromIntegral q)
        SDL.drawLine r (SDL.P p') (SDL.P q')


setColor :: SDL.Renderer -> Colour -> IO ()
setColor r c = SDL.rendererDrawColor r $= c
