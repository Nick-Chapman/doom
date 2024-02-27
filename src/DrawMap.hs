
module DrawMap
  ( draw
  ) where

import Pic (Pic(..))
import Wad (Wad(..),Level(..),Linedef(..),Vertex,V2(..),Int16)

draw :: Wad -> Pic ()
draw Wad{level1} = do
  drawLevel level1

drawLevel :: Level -> Pic ()
drawLevel Level{linedefs,vertexes} = do
  mapM_ drawLinedef linedefs
  mapM_ drawVertex vertexes -- vertexes on top of lines
  pure ()

unquantize :: V2 Int16 -> V2 Float -- TODO: wrong/silly!
unquantize = fmap fromIntegral

drawVertex :: Vertex -> Pic ()
drawVertex v = Dot (unquantize v)

drawLinedef :: Linedef -> Pic ()
drawLinedef Linedef{start,end} = do
  Line (unquantize start) (unquantize end)
