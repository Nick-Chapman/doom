module Top where

import qualified Engine
import qualified Wad
import Wad (Wad(..),Level(..))

main :: IO ()
main = do
  wad <- Wad.load "doom1.wad"
  let Wad{level1=Level{vertexes}} = wad
  print (length vertexes)
  let xs = map fst vertexes
  let ys = map snd vertexes
  let bb = ((minimum xs, minimum ys),(maximum xs,maximum ys))
  let conf = Engine.initConf bb
  Engine.run conf wad
