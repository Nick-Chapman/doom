
module Top (main) where

import Wad (Wad(..),Level(..),V2(..))
import qualified Engine
import qualified Wad (load)

main :: IO ()
main = do
  wad <- Wad.load "doom1.wad"
  let Wad{level1=Level{vertexes,nodes}} = wad
  let xs = [ x | V2 x _ <- vertexes ]
  let ys = [ y | V2 _ y <- vertexes ]
  let bb = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  let conf = Engine.initConf bb
  Engine.run conf wad
  let _ = mapM_ print (zip [0::Int ..] nodes)
  pure ()
