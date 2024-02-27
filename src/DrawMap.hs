
module DrawMap
  ( draw
  ) where

import Pic (Pic(..),V2(..))
import Wad (Wad(..),Level(..),Vertex)

draw :: Wad -> Pic ()
draw Wad{level1} = do
  drawLevel level1

drawLevel :: Level -> Pic ()
drawLevel Level{vertexes} = do
  mapM_ drawVertex vertexes

drawVertex :: Vertex -> Pic ()
drawVertex (V2 x y) =
  Dot (V2 (fromIntegral x) (fromIntegral y))
