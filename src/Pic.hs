
module Pic
  ( Pic(..)
  , V2(..)
  , RGB, rgb, Colour
  , white,grey,darkGrey,red,green,blue,yellow,magenta,cyan
  ) where

import Control.Monad (ap,liftM)
import Linear.V2 (V2(..))
import GHC.Word (Word8)
import Linear.V4 (V4(..))

data Pic a where
  Ret :: a -> Pic a
  Bind :: Pic a -> (a -> Pic b) -> Pic b
  Pause :: Pic ()
  Dot :: Colour -> V2 Float -> Pic ()
  Line :: Colour -> V2 Float -> V2 Float -> Pic ()
  Rect :: Colour -> V2 Float -> V2 Float -> Pic ()

instance Functor Pic where fmap = liftM
instance Applicative Pic where pure = Ret; (<*>) = ap
instance Monad Pic where (>>=) = Bind

type RGB = (Word8,Word8,Word8)
type Colour = V4 Word8

rgb :: RGB -> Colour
rgb (r,g,b) = V4 r g b 255

white,grey,darkGrey,red,green,blue,yellow,magenta,cyan :: Colour

white    = rgb (255,255,255)
grey     = rgb (70,70,70)
darkGrey = rgb (20,20,20)
red      = rgb (255,0,0)
green    = rgb (0,255,0)
blue     = rgb (0,0,255)
yellow   = rgb (255,255,0)
magenta  = rgb (255,0,255)
cyan     = rgb (0,255,255)
