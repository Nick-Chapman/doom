
module Pic
  ( Pic(..)
  , V2(..)
  , RGB, rgb, Colour
  ) where

import Control.Monad (ap,liftM)
import Linear.V2 (V2(..))
import GHC.Word (Word8)
import Linear.V4 (V4(..))

type RGB = (Word8,Word8,Word8)
type Colour = V4 Word8

rgb :: RGB -> Colour
rgb (r,g,b) = V4 r g b 255

data Pic a where
  Ret :: a -> Pic a
  Bind :: Pic a -> (a -> Pic b) -> Pic b
  Dot :: Colour -> V2 Float -> Pic ()
  Line :: Colour -> V2 Float -> V2 Float -> Pic ()

instance Functor Pic where fmap = liftM
instance Applicative Pic where pure = Ret; (<*>) = ap
instance Monad Pic where (>>=) = Bind
