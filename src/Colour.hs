
module Colour
  ( RGB, rgb, Colour
  , white,lightGrey,grey,darkGrey,red,green,blue,yellow,magenta,cyan
  , randomColour
  ) where

import GHC.Word (Word8)
import Linear.V4 (V4(..))
import System.Random (getStdRandom,randomR)

type RGB = (Word8,Word8,Word8)
type Colour = V4 Word8

rgb :: RGB -> Colour
rgb (r,g,b) = V4 r g b 255

white,lightGrey,grey,darkGrey,red,green,blue,yellow,magenta,cyan :: Colour

white     = rgb (255,255,255)
lightGrey = rgb (150,150,150)
grey      = rgb (70,70,70)
darkGrey  = rgb (20,20,20)
red       = rgb (255,0,0)
green     = rgb (0,255,0)
blue      = rgb (0,0,255)
yellow    = rgb (255,255,0)
magenta   = rgb (255,0,255)
cyan      = rgb (0,255,255)

randomColour :: IO Colour
randomColour = do
  r <- getStdRandom (randomR (100,255))
  g <- getStdRandom (randomR (100,255))
  b <- getStdRandom (randomR (100,255))
  pure $ rgb (r,g,b)
