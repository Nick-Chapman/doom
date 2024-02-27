
module Pic
  ( Pic(..)
  , V2(..)
  ) where

import Control.Monad (ap,liftM)
import Linear.V2 (V2(..))

data Pic a where
  Ret :: a -> Pic a
  Bind :: Pic a -> (a -> Pic b) -> Pic b
  -- TODO: Dot/Line should already be quantized to CInt?
  Dot :: V2 Float -> Pic ()
  Line :: V2 Float -> V2 Float -> Pic ()

instance Functor Pic where fmap = liftM
instance Applicative Pic where pure = Ret; (<*>) = ap
instance Monad Pic where (>>=) = Bind
