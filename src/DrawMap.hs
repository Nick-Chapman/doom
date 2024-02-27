
module DrawMap
  ( draw
  ) where

import Pic (Pic(..),rgb,Colour)
import Wad (Wad(..),Level(..),Thing(..),Linedef(..),Vertex,V2(..),Int16)

draw :: Wad -> Pic ()
draw Wad{level1,player} = do
  drawLevel level1
  drawPlayer 45 player

drawPlayer :: Int16 -> Thing -> Pic ()
drawPlayer h_fov Thing{pos,angle} = do
  let magenta = rgb (255,0,255)
  Dot magenta (unquantize pos)
  let len = 1000
  let a = angle
  let a1 = a - h_fov
  let a2 = a + h_fov
  drawVec magenta pos a len
  drawVec magenta pos a1 len
  drawVec magenta pos a2 len

drawVec :: Colour -> Vertex -> Int16 -> Float -> Pic ()
drawVec col pos angle len = do
  let p = unquantize pos
  let r = radians angle
  let vec = V2 (len * cos r) (len * sin r)
  Line col p (p+vec)
  where
    radians n = fromIntegral n * pi / 180

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
