module Top where

import qualified Engine
import qualified Wad

main :: IO ()
main = do
  wad <- Wad.load "doom1.wad"
  Engine.run wad
