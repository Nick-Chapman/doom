module Top where

import qualified Engine
import qualified Wad
import Wad (Wad(..),Level(..),V2(..))

main :: IO ()
main = do
  wad <- Wad.load "doom1.wad"
  let Wad{level1=Level{vertexes,linedefs},player} = wad
  print (length vertexes, length linedefs, player)
  let xs = [ x | V2 x _ <- vertexes ]
  let ys = [ y | V2 _ y <- vertexes ]
  let bb = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  let conf = Engine.initConf bb
  Engine.run conf wad
