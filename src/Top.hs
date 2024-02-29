
module Top (main) where

import Pic
import System.Random (getStdRandom,randomR)
import Wad (Wad(..),Level(..))
import qualified Engine
import qualified Wad (load)

main :: IO ()
main = do
  randCols <- sequence (replicate 100 randomCol)
  wad <- Wad.load "doom1.wad"
  let Wad{level1=Level{vertexes,nodes}} = wad
  let xs = [ x | V2 x _ <- vertexes ]
  let ys = [ y | V2 _ y <- vertexes ]
  let bb = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  let conf = Engine.initConf bb randCols
  Engine.run conf wad
  let _ = mapM_ print (zip [0::Int ..] nodes)
  pure ()

randomCol :: IO Colour
randomCol = do
  r <- getStdRandom (randomR (100,255))
  g <- getStdRandom (randomR (100,255))
  b <- getStdRandom (randomR (100,255))
  pure $ rgb (r,g,b)
