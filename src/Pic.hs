
module Pic
  ( Pic(..)
  , Point
  ) where

import Control.Monad (ap,liftM)

--type Point = (Double,Double)
type Point = (Float,Float) -- less guff when debug printing

data Pic a where
  Ret :: a -> Pic a
  Bind :: Pic a -> (a -> Pic b) -> Pic b
  Dot :: Point -> Pic ()

instance Functor Pic where fmap = liftM
instance Applicative Pic where pure = Ret; (<*>) = ap
instance Monad Pic where (>>=) = Bind
