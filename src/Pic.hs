
module Pic
  ( Pic(..)
  , V2(..)
  ) where

import Control.Monad (ap,liftM)
import Linear.V2 (V2(..))
import Colour

data Pic a where
  Ret :: a -> Pic a
  Bind :: Pic a -> (a -> Pic b) -> Pic b
  Pause :: Pic ()
  Mes :: String -> Pic ()
  Eff :: IO () -> Pic ()
  Dot :: Colour -> V2 Float -> Pic ()
  Line :: Colour -> V2 Float -> V2 Float -> Pic ()
  Rect :: Colour -> V2 Float -> V2 Float -> Pic ()
  LineQ :: Colour -> V2 Int -> V2 Int -> Pic ()

instance Functor Pic where fmap = liftM
instance Applicative Pic where pure = Ret; (<*>) = ap
instance Monad Pic where (>>=) = Bind
