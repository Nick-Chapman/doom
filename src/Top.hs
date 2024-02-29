
module Top (main) where

import Pic (Colour,rgb,V2(..))
import System.Random (getStdRandom,randomR)
import Wad (Wad(..),Level(..))
import qualified Engine (initConf,run)
import qualified Wad (load)

main :: IO ()
main = do
  -- TODO: seed random for same colours each time
  randCols <- sequence (replicate 1000 randomCol)
  wad <- Wad.load "doom1.wad"
  let Wad{level1=Level{vertexes}} = wad
  let xs = [ x | V2 x _ <- vertexes ]
  let ys = [ y | V2 _ y <- vertexes ]
  let bb = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  let conf = Engine.initConf bb randCols
  Engine.run conf wad
  pure ()

randomCol :: IO Colour
randomCol = do
  r <- getStdRandom (randomR (100,255))
  g <- getStdRandom (randomR (100,255))
  b <- getStdRandom (randomR (100,255))
  pure $ rgb (r,g,b)
