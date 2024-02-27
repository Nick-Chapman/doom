
module DrawMap
  ( draw
  ) where

import Pic (Pic(..),rgb)
import Wad (Wad(..),Level(..),Linedef(..),Vertex,V2(..),Int16)

draw :: Wad -> Pic ()
draw Wad{level1} = do
  drawLevel level1

drawLevel :: Level -> Pic ()
drawLevel Level{linedefs,vertexes} = do
  mapM_ drawLinedef linedefs
  mapM_ drawVertex vertexes -- vertexes on top of lines
  pure ()

unquantize :: V2 Int16 -> V2 Float
unquantize = fmap fromIntegral

drawVertex :: Vertex -> Pic ()
drawVertex v = do
  let yellow = rgb (255,255,0)
  Dot yellow (unquantize v)

drawLinedef :: Linedef -> Pic ()
drawLinedef Linedef{start,end} = do
  let red = rgb (255,0,0)
  Line red (unquantize start) (unquantize end)
