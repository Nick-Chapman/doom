
module Top (main) where

import Colour (randomColour)
import Pic (V2(..))
import System.Random (setStdGen,mkStdGen)
import Wad (Wad(..),Level(..))
import qualified Engine (initConf,run)
import qualified Wad (load)

main :: IO ()
main = do
  setStdGen (mkStdGen 0) -- fixed seed; same random colours each run
  randCols <- sequence (replicate 1000 randomColour)
  wad <- Wad.load "doom1.wad"
  let Wad{level1=Level{vertexes}} = wad
  let xs = [ x | V2 x _ <- vertexes ]
  let ys = [ y | V2 _ y <- vertexes ]
  let bb = (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  let conf = Engine.initConf bb randCols
  Engine.run conf wad
  pure ()
