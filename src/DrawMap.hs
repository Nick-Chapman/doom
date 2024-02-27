
module DrawMap
  ( draw
  ) where

import Pic (Pic(..),rgb,Colour)
import Wad (Wad(..),Level(..),Thing(..),Linedef(..),Vertex,V2(..),Int16
           ,Node(..),Subsector(..),Seg(..))
import Data.Word (Word16)
import Data.Bits (shiftL)

draw :: Wad -> Pic ()
draw Wad{level1,player} = do
  let lds = collectLDsL level1
  let pickLD n = n `elem` lds
  drawLevel pickLD level1
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

drawLevel :: (Int16 -> Bool) -> Level -> Pic ()
drawLevel pickLD Level{linedefs,vertexes} = do
  let red = rgb (255,0,0)
  let green = rgb (0,255,0)
  let col ld = if pickLD ld then green else red
  sequence_ [ drawLinedef ld (col i) | (i,ld) <- zip [0..] linedefs ]
  mapM_ drawVertex vertexes -- vertexes on top of lines
  pure ()

unquantize :: V2 Int16 -> V2 Float
unquantize = fmap fromIntegral

drawVertex :: Vertex -> Pic ()
drawVertex v = do
  let yellow = rgb (255,255,0)
  Dot yellow (unquantize v)

drawLinedef :: Linedef -> Colour -> Pic ()
drawLinedef Linedef{start,end} col = do
  Line col (unquantize start) (unquantize end)

collectLDsL :: Level -> [Int16]
collectLDsL Level{segs,subsectors,nodes} = tNodeJustL [] (nodes!!max)
  where
    max = length nodes - 1

    tNodeJustL :: [Int16] -> Node -> [Int16]
    tNodeJustL acc Node{left} = tId acc left

    tNode :: [Int16] -> Node -> [Int16]
    tNode acc Node{right,left} = tId (tId acc left) right

    tId :: [Int16] -> Int16 -> [Int16]
    tId acc id =
      if id >= 0 then tNode acc (nodes!! fromIntegral id) else do
        let ssId :: Word16 = (1 `shiftL` 15) + fromIntegral id
        let Subsector{first,count} = subsectors!!(fromIntegral ssId)
        let sssegs = [ segs!!(fromIntegral i) | i <- [first..first+count-1] ]
        let lds = [ ld | Seg{linedefId=ld} <- sssegs ]
        lds ++ acc
